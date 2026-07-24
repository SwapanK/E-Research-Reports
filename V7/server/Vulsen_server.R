# =============================================================================
# server/Vulsen_server.R
# VulSen server - full module with Legend Configuration Manager, gallery
# overrides, per-card sizing, and downloads
# =============================================================================

Vulsen_server <- function(id, cart, username) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Fixed on-screen rendering resolution (px per inch).
    VULSEN_SCREEN_DPI <- 96
    
    # -------------------------------------------------------------------------
    # Reactive values
    # -------------------------------------------------------------------------
    rv <- shiny::reactiveValues(
      raw_data = NULL,
      obj = NULL,
      plots = NULL,
      bundles = NULL,
      model_loaded = FALSE,
      data_loaded = FALSE,
      plots_generated = FALSE,
      downloaded = FALSE,
      n_region = 0L,
      n_state = 0L,
      n_pct = 0L,
      header_error = FALSE,
      header_error_msg = ""
    )
    
    # Default overrides per group (region, state, pct)
    defaults <- list(
      region = shiny::reactiveVal(vul_default_overrides("region")),
      state  = shiny::reactiveVal(vul_default_overrides("state")),
      pct    = shiny::reactiveVal(vul_default_overrides("pct"))
    )
    
    # -------------------------------------------------------------------------
    # Legend Configuration Manager - staging (editable) and committed (plots)
    # -------------------------------------------------------------------------
    rel_scheme_staging <- shiny::reactiveVal(REL_AAL_DEFAULT_BINS)
    rel_scheme_committed <- shiny::reactiveVal(REL_AAL_DEFAULT_BINS)
    pct_scheme_staging <- shiny::reactiveVal(PCT_CHANGE_DEFAULT_BINS)
    pct_scheme_committed <- shiny::reactiveVal(PCT_CHANGE_DEFAULT_BINS)
    
    legend_msg <- list(
      rel = shiny::reactiveVal(NULL),
      pct = shiny::reactiveVal(NULL)
    )
    
    # ---- Helper: wire up one legend tab's full set of observers ----
    setup_legend_manager <- function(tag, scheme_staging, committed_scheme, default_bins, scheme_type, value_source) {
      # tag = "rel" or "pct"
      
      # -- Editable table render (with colour swatches) using staging --
      output[[paste0(tag, "_legend_table")]] <- DT::renderDT({
        vul_legend_datatable(scheme_staging())
      }, server = FALSE)
      
      # -- Colour picker: row click observer --
      shiny::observeEvent(input[[paste0(tag, "_colour_row")]], {
        row <- input[[paste0(tag, "_colour_row")]]
        if (is.null(row) || !is.numeric(row)) return()
        df <- scheme_staging()
        df <- df[order(-df$level), ]
        if (row < 1 || row > nrow(df)) return()
        current_colour <- df$colour[row]
        
        shiny::showModal(
          shiny::modalDialog(
            title = paste("Pick a colour for bin", row),
            colourpicker::colourInput(
              inputId = ns(paste0(tag, "_colour_picker")),
              label = NULL,
              value = current_colour,
              allowTransparent = FALSE,
              showColour = "both"
            ),
            footer = shiny::tagList(
              shiny::actionButton(ns(paste0(tag, "_colour_apply")), "Apply", class = "btn-primary"),
              shiny::modalButton("Cancel")
            ),
            easyClose = TRUE
          )
        )
      })
      
      # -- Apply colour change --
      shiny::observeEvent(input[[paste0(tag, "_colour_apply")]], {
        new_colour <- input[[paste0(tag, "_colour_picker")]]
        row <- input[[paste0(tag, "_colour_row")]]
        if (is.null(row) || is.null(new_colour)) return()
        df <- scheme_staging()
        df <- df[order(-df$level), ]
        if (row < 1 || row > nrow(df)) return()
        df$colour[row] <- new_colour
        v <- vul_validate_scheme(df)
        if (isTRUE(v$ok)) {
          scheme_staging(v$scheme)
          shiny::removeModal()
          legend_msg[[tag]](NULL)
          shiny::showNotification("Colour updated (apply to plots to see changes)", type = "message", duration = 2.5)
        } else {
          shiny::showNotification("Invalid colour", type = "error")
        }
      })
      
      # -- Cell edits (Lower column) with automatic label recalculation --
      shiny::observeEvent(input[[paste0(tag, "_legend_table_cell_edit")]], {
        info <- input[[paste0(tag, "_legend_table_cell_edit")]]
        df <- scheme_staging()
        df <- df[order(-suppressWarnings(as.numeric(df$level))), c("level", "label", "lower", "upper", "colour")]
        col_names <- c("level", "label", "lower", "upper", "colour")
        col <- col_names[info$col + 1]
        row <- info$row
        val <- info$value
        if (col %in% c("lower", "upper")) val <- suppressWarnings(as.numeric(val))
        df[row, col] <- val
        
        # Auto‑sync neighbour edges
        if (col == "lower" && row < nrow(df)) {
          df[row + 1, "upper"] <- val
        } else if (col == "upper" && row > 1) {
          df[row - 1, "lower"] <- val
        }
        
        # Recompute labels — but only when the edit wasn't to the Label
        # column itself. Otherwise a manually typed label gets immediately
        # clobbered by the auto-generated one before scheme_staging() (and
        # therefore the Legend Preview, which reads scheme_staging()) ever
        # sees it.
        if (col != "label") {
          sentinel_low <- -1e12
          sentinel_high <- 1e12
          fmt <- function(x) {
            vapply(x, function(v) {
              if (!is.finite(v)) as.character(v) else format(round(v, 2), nsmall = 0, trim = TRUE)
            }, character(1))
          }
          df$label <- ifelse(
            df$lower == sentinel_low, paste0("<", fmt(df$upper)),
            ifelse(df$upper == sentinel_high, paste0(">", fmt(df$lower)),
                   paste0(fmt(df$lower), " - ", fmt(df$upper)))
          )
        }
        df <- df[order(df$level), ]
        v <- vul_validate_scheme(df)
        if (isTRUE(v$ok)) {
          scheme_staging(v$scheme)
          legend_msg[[tag]](NULL)
          shiny::showNotification("Legend updated (apply to plots to see changes)", type = "message", duration = 2)
        } else {
          legend_msg[[tag]](v$msg)
        }
      })
      
      # -- Load Default --
      shiny::observeEvent(input[[paste0(tag, "_legend_load_default")]], {
        scheme_staging(default_bins)
        legend_msg[[tag]](NULL)
        shiny::showNotification("Default legend loaded (apply to plots to see changes)", type = "message", duration = 2)
      })
      
      # -- Load JSON --
      shiny::observeEvent(input[[paste0(tag, "_legend_json_file")]], {
        f <- input[[paste0(tag, "_legend_json_file")]]
        shiny::req(f)
        txt <- tryCatch(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"), error = function(e) NULL)
        if (is.null(txt)) {
          legend_msg[[tag]]("Could not read the uploaded file.")
          return(invisible(NULL))
        }
        res <- vul_scheme_from_json(txt)
        if (!isTRUE(res$ok)) {
          legend_msg[[tag]](res$msg)
          return(invisible(NULL))
        }
        scheme_staging(res$scheme)
        legend_msg[[tag]](NULL)
        shiny::showNotification("Legend JSON loaded (apply to plots to see changes)", type = "message", duration = 2)
      })
      
      # -- Download JSON --
      output[[paste0(tag, "_legend_download_json")]] <- shiny::downloadHandler(
        filename = function() paste0("vulsen_", tag, "_legend_", Sys.Date(), ".json"),
        content = function(file) {
          json <- vul_scheme_to_json(scheme_staging(), paste0(tag, "_scheme"), scheme_type)
          writeLines(as.character(json), file)
        }
      )
      
      # -- Create Bins --
      shiny::observeEvent(input[[paste0(tag, "_legend_create_bins")]], {
        vals <- value_source()
        if (is.null(vals) || !length(vals)) {
          legend_msg[[tag]]("Generate plots first (Section 3) so there is data to bin.")
          return(invisible(NULL))
        }
        n_bins <- input[[paste0(tag, "_legend_n_bins")]] %||% 9
        res <- tryCatch(vul_quantile_bins(vals, n_bins), error = function(e) e)
        if (inherits(res, "error")) {
          legend_msg[[tag]](conditionMessage(res))
          return(invisible(NULL))
        }
        scheme_staging(res)
        legend_msg[[tag]](NULL)
        shiny::showNotification(paste("Created", nrow(res), "quantile bins (apply to plots to see changes)"), type = "message", duration = 2.5)
      })
      
      # -- Validation message --
      output[[paste0(tag, "_legend_msg")]] <- shiny::renderUI({
        msg <- legend_msg[[tag]]()
        if (is.null(msg)) return(NULL)
        status_box("error", msg)
      })
      
      # -- Live preview swatches (using staging) --
      output[[paste0(tag, "_legend_preview")]] <- shiny::renderUI({
        vul_legend_preview_tags(scheme_staging())
      })
      
      # Keep table/preview active even when hidden
      shiny::outputOptions(output, paste0(tag, "_legend_table"), suspendWhenHidden = FALSE)
      shiny::outputOptions(output, paste0(tag, "_legend_preview"), suspendWhenHidden = FALSE)
    }
    
    # -------------------------------------------------------------------------
    # Setup both legend managers
    # -------------------------------------------------------------------------
    setup_legend_manager(
      "rel", rel_scheme_staging, rel_scheme_committed, REL_AAL_DEFAULT_BINS, "relative_aal",
      value_source = function() {
        st <- obj_state()
        if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
        unlist(lapply(st$obj$region_split, function(d) d$value), use.names = FALSE)
      }
    )
    setup_legend_manager(
      "pct", pct_scheme_staging, pct_scheme_committed, PCT_CHANGE_DEFAULT_BINS, "percentage_change",
      value_source = function() {
        if (!isTRUE(rv$plots_generated)) return(NULL)
        pd <- rv$plots$pct_data
        if (is.null(pd) || !length(pd)) return(NULL)
        unlist(lapply(pd, function(d) d$value), use.names = FALSE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Sample data paths
    # -------------------------------------------------------------------------
    sample_moody_path <- file.path("data", "VulsenAPP_sample", "USSCS_RMS_HDv1_DLM_AAL_AllPeril_vulsen_comp_CCMasonary_SAMPLE.csv")
    sample_verisk_path <- file.path("data", "VulsenAPP_sample", "USSCS_Verisk_v12v13_vulsen_comp_AllPeril_SAMPLE.csv")
    
    # -------------------------------------------------------------------------
    # Status helpers
    # -------------------------------------------------------------------------
    output$model_status <- shiny::renderUI({
      if (rv$model_loaded) tags$span(class = "sec2-status done", icon("check"), "Done")
      else tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    })
    
    output$data_status <- shiny::renderUI({
      if (rv$data_loaded) tags$span(class = "sec2-status done", icon("check"), "Done")
      else tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    })
    
    output$plot_status <- shiny::renderUI({
      if (rv$plots_generated) tags$span(class = "sec2-status done", icon("check"), "Done")
      else tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    })
    
    output$download_status <- shiny::renderUI({
      if (rv$downloaded) tags$span(class = "sec2-status done", icon("check"), "Done")
      else tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    })
    
    # Progress stepper
    output$progress_panel <- shiny::renderUI({
      stages <- list(
        list(label = "Model",    done = rv$model_loaded),
        list(label = "Data",     done = rv$data_loaded),
        list(label = "Plots",    done = rv$plots_generated),
        list(label = "Download", done = rv$downloaded)
      )
      
      div(
        class = "sec-progress-panel",
        style = "margin: 16px 0 12px 0; padding: 12px 16px; background: #f8faff; border-radius: 16px; border: 1px solid #e2e8f0;",
        div(
          style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
          lapply(seq_along(stages), function(i) {
            st <- stages[[i]]
            is_done <- st$done
            circle <- if (is_done) tags$span(class = "sec-step-circle done", icon("check")) else tags$span(class = "sec-step-circle pending", i)
            label <- tags$div(class = "sec-step-label", st$label)
            connector <- if (i < length(stages)) {
              tags$div(class = paste("sec-connector", if (is_done && stages[[i + 1]]$done) "done" else "pending"))
            } else NULL
            tags$div(
              class = "sec-step-clickable",
              style = "display: flex; align-items: center; flex: 1;",
              onclick = sprintf("scrollToSection('%s')", ns(paste0("step_", tolower(st$label)))),
              tags$div(style = "display: flex; flex-direction: column; align-items: center;", circle, label),
              connector
            )
          })
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Model column name fields
    # -------------------------------------------------------------------------
    output$model_columns_ui <- shiny::renderUI({
      vendor <- input$model_family %||% "verisk"
      n_models <- input$n_models %||% "2"
      
      m1_default <- MODEL1_COL_DEFAULT[[vendor]] %||% "v13"
      m2_default <- MODEL2_COL_DEFAULT[[vendor]] %||% "v12"
      
      if (identical(n_models, "1")) {
        shiny::tagList(
          shiny::textInput(inputId = ns("model1_col"), label = "Model-1 AAL column name", value = m1_default, width = "100%")
        )
      } else {
        shiny::tagList(
          shiny::textInput(inputId = ns("model1_col"), label = "Model-1 AAL column name", value = m1_default, width = "100%"),
          shiny::textInput(inputId = ns("model2_col"), label = "Model-2 AAL column name", value = m2_default, width = "100%")
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Version text
    # -------------------------------------------------------------------------
    output$version_text <- shiny::renderUI({
      m1 <- input$model1_col %||% ""
      if (identical(input$n_models %||% "2", "1")) {
        shiny::tags$strong(paste("Version:", m1))
      } else {
        m2 <- input$model2_col %||% ""
        shiny::tags$strong(paste("Versions:", m1, "vs", m2))
      }
    })
    
    # -------------------------------------------------------------------------
    # Show/hide the Percentage Change tab
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$n_models, {
      if (identical(input$n_models, "2")) {
        shiny::showTab(inputId = "vulsen_tabs", target = "pct_tab")
      } else {
        shiny::hideTab(inputId = "vulsen_tabs", target = "pct_tab")
      }
    }, ignoreNULL = FALSE)
    
    # -------------------------------------------------------------------------
    # Data load state
    # -------------------------------------------------------------------------
    sample_loaded <- shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(input$load_sample, {
      sample_loaded(TRUE)
    })
    
    raw_state <- shiny::reactive({
      if (!is.null(input$file)) {
        return(tryCatch({
          df <- read_vulsens_file(input$file$datapath)
          list(ok = TRUE, msg = paste("Loaded uploaded file:", input$file$name), data = df)
        }, error = function(e) {
          list(ok = FALSE, msg = paste("Could not read file:", conditionMessage(e)), data = NULL)
        }))
      }
      
      if (isTRUE(sample_loaded())) {
        path <- if (identical(input$model_family, "moody")) sample_moody_path else sample_verisk_path
        return(tryCatch({
          df <- read_vulsens_file(path)
          list(ok = TRUE, msg = paste("Loaded sample data for", ifelse(identical(input$model_family, "moody"), "Moody's / RMS", "Verisk")), data = df)
        }, error = function(e) {
          list(ok = FALSE, msg = paste("Could not load sample data:", conditionMessage(e)), data = NULL)
        }))
      }
    })
    
    # -------------------------------------------------------------------------
    # Header validation (Problem 3)
    # -------------------------------------------------------------------------
    shiny::observe({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        rv$header_error <- FALSE
        rv$header_error_msg <- ""
        return()
      }
      # Build expected headers based on current configuration
      expected <- get_expected_headers(
        model_family = input$model_family,
        model1_col = input$model1_col,
        model2_col = if (identical(input$n_models, "2")) input$model2_col else NULL
      )
      actual <- trimws(names(rs$data))
      missing <- setdiff(expected, actual)
      extra   <- setdiff(actual, expected)
      
      if (length(missing) == 0 && length(extra) == 0) {
        rv$header_error <- FALSE
        rv$header_error_msg <- ""
      } else {
        msg <- paste0(
          "The uploaded CSV must contain exactly these column headers:\n",
          paste(expected, collapse = ", "), "\n\n"
        )
        if (length(missing) > 0) {
          msg <- paste0(msg, "Missing columns: ", paste(missing, collapse = ", "), "\n")
        }
        if (length(extra) > 0) {
          msg <- paste0(msg, "Extra columns: ", paste(extra, collapse = ", "))
        }
        rv$header_error <- TRUE
        rv$header_error_msg <- msg
        
        # Show modal dialog
        shiny::showModal(
          shiny::modalDialog(
            title = "Invalid CSV Headers",
            shiny::tags$pre(style = "white-space: pre-wrap;", msg),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Dynamic subperil update (Problem 4)
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$peril, {
      choices <- peril_lookup()[[input$peril]]
      if (!is.null(choices)) {
        shiny::updateSelectInput(session, "subperil", choices = choices, selected = choices[1])
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # -------------------------------------------------------------------------
    # Build analysis object
    # -------------------------------------------------------------------------
    obj_state <- shiny::reactive({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        return(list(ok = FALSE, msg = rs$msg, obj = NULL))
      }
      # If header error, do not proceed
      if (rv$header_error) {
        return(list(ok = FALSE, msg = "Header validation failed. Please correct the file and re-upload.", obj = NULL))
      }
      tryCatch({
        n_models <- input$n_models %||% "2"
        model2_col <- if (identical(n_models, "2")) input$model2_col else NULL
        meta <- list(
          country_code = input$country_code %||% DEFAULT_COUNTRY_CODE,
          peril = input$peril %||% "",
          subperil = input$subperil %||% "",
          suffix = input$suffix %||% DEFAULT_SUFFIX
        )
        o <- build_rmd_objects(rs$data, input$model_family, input$model1_col, model2_col, meta)
        rv$model_loaded <- TRUE
        rv$data_loaded <- TRUE
        list(ok = TRUE, msg = rs$msg, obj = o)
      }, error = function(e) {
        rv$model_loaded <- FALSE
        rv$data_loaded <- FALSE
        list(ok = FALSE, msg = paste("Validation failed:", conditionMessage(e), "Please check model selection and column names in the uploaded file."), obj = NULL)
      })
    })
    
    # -------------------------------------------------------------------------
    # Status / Metrics UI (Problem 1: hidden)
    # -------------------------------------------------------------------------
    # output$status_ui is no longer rendered
    
    output$metrics_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      o <- st$obj
      shiny::fluidRow(
        shiny::column(3, metric_card(format(nrow(o$final_comp), big.mark = ","), "Input rows")),
        shiny::column(3, metric_card(length(unique(o$final_comp$STATECODE)), "States")),
        shiny::column(3, metric_card(length(unique(o$final_comp$Region)), "Regions")),
        shiny::column(3, metric_card(o$model_title, "Model family"))
      )
    })
    
    # -------------------------------------------------------------------------
    # Static ggplots - now use committed schemes
    # -------------------------------------------------------------------------
    gen_trigger <- shiny::reactiveVal(0)
    shiny::observeEvent(input$create_plot, {
      gen_trigger(gen_trigger() + 1)
      rv$downloaded <- FALSE
    })
    
    static_plots_rel <- shiny::reactive({
      shiny::req(gen_trigger() > 0)
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      build_rel_state_gplots(st$obj, side_by_side = FALSE, rel_scheme = rel_scheme_committed())
    })
    
    static_plots_pct <- shiny::reactive({
      shiny::req(gen_trigger() > 0)
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      build_pct_gplots(st$obj, pct_scheme = pct_scheme_committed())
    })
    
    static_plots <- shiny::reactive({
      rs <- static_plots_rel()
      pc <- static_plots_pct()
      list(region = rs$region, state = rs$state, pct = pc$pct, pct_data = pc$pct_data)
    })
    
    # -------------------------------------------------------------------------
    # Build bundles and set up plot rendering
    # -------------------------------------------------------------------------
    shiny::observe({
      plots <- static_plots()
      shiny::req(plots)
      
      rv$plots <- plots
      
      for (group in c("region", "state", "pct")) {
        gp <- plots[[group]]
        if (is.null(gp) || length(gp) == 0) next
        def <- defaults[[group]]()
        bundles <- list()
        for (nm in names(gp)) {
          bundles[[nm]] <- list(
            plot = gp[[nm]],
            width = def$width_in, height = def$height_in, dpi = def$dpi,
            transparent_bg = def$transparent_bg
          )
        }
        if (is.null(rv$bundles)) rv$bundles <- list()
        rv$bundles[[group]] <- bundles
        
        n_field <- paste0("n_", group)
        n_new <- length(gp)
        if (!identical(shiny::isolate(rv[[n_field]]), n_new)) {
          rv[[n_field]] <- n_new
        }
      }
      
      if (!isTRUE(shiny::isolate(rv$plots_generated))) rv$plots_generated <- TRUE
      
      # Render plot outputs for each group
      for (group in c("region", "state", "pct")) {
        local({
          grp <- group
          plot_list <- rv$plots[[grp]]
          if (is.null(plot_list) || length(plot_list) == 0) return()
          
          for (nm in names(plot_list)) {
            local({
              key <- nm
              safe_key <- sec_safe_id_key(key)
              prefix <- grp
              
              # Plot frame UI
              output[[paste0(prefix, "_plot_frame_", safe_key)]] <- shiny::renderUI({
                def <- defaults[[prefix]]()
                width_in  <- input[[paste0(prefix, "_width_in_", safe_key)]]  %||% def$width_in  %||% 9
                height_in <- input[[paste0(prefix, "_height_in_", safe_key)]] %||% def$height_in %||% 5
                width_px  <- max(200, round(width_in  * VULSEN_SCREEN_DPI))
                height_px <- max(150, round(height_in * VULSEN_SCREEN_DPI))
                shiny::plotOutput(
                  ns(paste0(prefix, "_plot_", safe_key)),
                  width = paste0(width_px, "px"),
                  height = paste0(height_px, "px")
                )
              })
              
              # Main plot render
              output[[paste0(prefix, "_plot_", safe_key)]] <- shiny::renderPlot({
                base_plot <- rv$plots[[prefix]][[key]]
                shiny::req(base_plot)
                def <- defaults[[prefix]]()
                p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def)
                p
              },
              width = function() {
                def <- defaults[[prefix]]()
                width_in <- input[[paste0(prefix, "_width_in_", safe_key)]] %||% def$width_in %||% 9
                max(200, round(width_in * VULSEN_SCREEN_DPI))
              },
              height = function() {
                def <- defaults[[prefix]]()
                height_in <- input[[paste0(prefix, "_height_in_", safe_key)]] %||% def$height_in %||% 5
                max(150, round(height_in * VULSEN_SCREEN_DPI))
              },
              res = VULSEN_SCREEN_DPI,
              bg = "transparent")
              
              # Download handler
              output[[paste0(prefix, "_dl_", safe_key)]] <- shiny::downloadHandler(
                filename = function() paste0("vulsen_", prefix, "_", safe_key, "_", Sys.Date(), ".png"),
                content = function(file) {
                  base_plot <- rv$plots[[prefix]][[key]]
                  shiny::req(base_plot)
                  def <- defaults[[prefix]]()
                  p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def)
                  w   <- attr(p, "vulsen_width_in")  %||% def$width_in  %||% 9
                  h   <- attr(p, "vulsen_height_in") %||% def$height_in %||% 5
                  dpi <- attr(p, "vulsen_dpi")       %||% def$dpi       %||% 300
                  bg  <- attr(p, "vulsen_bg")        %||% "white"
                  ggplot2::ggsave(file, plot = p, width = w, height = h, dpi = dpi, bg = bg, limitsize = FALSE)
                  rv$downloaded <- TRUE
                }
              )
            })
          }
        })
      }
      
      # ---- Force a re-sync of colour swatches after dynamic UI updates ----
      session$sendCustomMessage("sync-colour-swatches", list())
    })
    
    # -------------------------------------------------------------------------
    # Add to cart
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$vul_cart_click, {
      key_full <- input$vul_cart_click$key
      parts    <- strsplit(key_full, "\\|")[[1]]
      prefix   <- parts[1]
      safe_key <- parts[2]
      
      plot_names <- names(rv$plots[[prefix]])
      shiny::req(plot_names)
      match_idx <- which(vapply(plot_names, sec_safe_id_key, character(1)) == safe_key)
      shiny::req(length(match_idx) >= 1)
      key <- plot_names[match_idx[1]]
      
      base_plot <- rv$plots[[prefix]][[key]]
      shiny::req(base_plot)
      
      def <- defaults[[prefix]]()
      p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def)
      
      w   <- attr(p, "vulsen_width_in")  %||% def$width_in  %||% 9
      h   <- attr(p, "vulsen_height_in") %||% def$height_in %||% 5
      dpi <- attr(p, "vulsen_dpi")       %||% def$dpi       %||% 300
      
      item <- list(
        id         = generate_item_id(),
        module     = "Vulnerability",
        plot       = p,
        commentary = NULL,
        timestamp  = Sys.time(),
        width      = w,
        height     = h,
        dpi        = dpi
      )
      
      current_cart <- cart()
      current_cart[[length(current_cart) + 1]] <- item
      cart(current_cart)
      
      shiny::showNotification("Added to cart", type = "message", duration = 2)
    })
    
    # -------------------------------------------------------------------------
    # "Apply to all" observers - now also commit the legend staging
    # -------------------------------------------------------------------------
    for (group in c("region", "state", "pct")) {
      local({
        grp <- group
        
        shiny::observeEvent(input[[paste0(grp, "_apply_all")]], {
          def <- list(
            width_in = input[[paste0(grp, "_default_width_in")]] %||% 9,
            height_in = input[[paste0(grp, "_default_height_in")]] %||% 5,
            dpi = input[[paste0(grp, "_default_dpi")]] %||% 300,
            transparent_bg = input[[paste0(grp, "_default_transparent_bg")]] %||% FALSE,
            panel_gap_px = input[[paste0(grp, "_default_panel_gap_px")]] %||% 16,
            top_margin_px = input[[paste0(grp, "_default_top_margin_px")]] %||% 10,
            bottom_margin_px = input[[paste0(grp, "_default_bottom_margin_px")]] %||% 10,
            left_margin_px = input[[paste0(grp, "_default_left_margin_px")]] %||% 10,
            right_margin_px = input[[paste0(grp, "_default_right_margin_px")]] %||% 10,
            axis_text = input[[paste0(grp, "_default_axis_text")]] %||% 12,
            plot_title = input[[paste0(grp, "_default_plot_title")]] %||% 16,
            strip_text = input[[paste0(grp, "_default_strip_text")]] %||% 12,
            legend_text = input[[paste0(grp, "_default_legend_text")]] %||% 10,
            legend_title = input[[paste0(grp, "_default_legend_title")]] %||% 10,
            x_rotation = input[[paste0(grp, "_default_x_rotation")]] %||% 0,
            x_vjust = input[[paste0(grp, "_default_x_vjust")]] %||% 0.5,
            legend_show = input[[paste0(grp, "_legend_show")]] %||% TRUE,
            legend_key_size = input[[paste0(grp, "_default_legend_key_size")]] %||% 0.8,
            show_labels = input[[paste0(grp, "_default_show_labels")]] %||% TRUE,
            data_label_size = input[[paste0(grp, "_default_data_label_size")]] %||% 3.5,
            data_label_colour = input[[paste0(grp, "_default_data_label_colour")]] %||% "#FFFFFF"
          )
          
          defaults[[grp]](def)
          
          plot_names <- names(shiny::isolate(rv$plots[[grp]]))
          if (is.null(plot_names) || length(plot_names) == 0) return()
          
          for (key in plot_names) {
            safe_key <- sec_safe_id_key(key)
            shiny::updateNumericInput(session, paste0(grp, "_width_in_", safe_key), value = def$width_in)
            shiny::updateNumericInput(session, paste0(grp, "_height_in_", safe_key), value = def$height_in)
            shiny::updateNumericInput(session, paste0(grp, "_dpi_", safe_key), value = def$dpi)
            shiny::updateCheckboxInput(session, paste0(grp, "_transparent_bg_", safe_key), value = def$transparent_bg)
            shiny::updateNumericInput(session, paste0(grp, "_panel_gap_px_", safe_key), value = def$panel_gap_px)
            shiny::updateNumericInput(session, paste0(grp, "_axis_text_", safe_key), value = def$axis_text)
            shiny::updateNumericInput(session, paste0(grp, "_x_rotation_", safe_key), value = def$x_rotation)
            shiny::updateNumericInput(session, paste0(grp, "_x_vjust_", safe_key), value = def$x_vjust)
            shiny::updateNumericInput(session, paste0(grp, "_plot_title_", safe_key), value = def$plot_title)
            if (grp != "pct") {
              shiny::updateNumericInput(session, paste0(grp, "_strip_text_", safe_key), value = def$strip_text)
            }
            shiny::updateNumericInput(session, paste0(grp, "_legend_text_", safe_key), value = def$legend_text)
            shiny::updateNumericInput(session, paste0(grp, "_legend_title_", safe_key), value = def$legend_title)
            shiny::updateCheckboxInput(session, paste0(grp, "_legend_show_", safe_key), value = def$legend_show)
            shiny::updateNumericInput(session, paste0(grp, "_legend_key_size_", safe_key), value = def$legend_key_size)
            shiny::updateCheckboxInput(session, paste0(grp, "_show_labels_", safe_key), value = def$show_labels)
            shiny::updateNumericInput(session, paste0(grp, "_data_label_size_", safe_key), value = def$data_label_size)
            colourpicker::updateColourInput(session, paste0(grp, "_data_label_colour_", safe_key), value = def$data_label_colour)
          }
          
          # ---- Apply legend staging to committed based on group ----
          if (grp %in% c("region", "state")) {
            rel_scheme_committed(rel_scheme_staging())
            shiny::showNotification("Relative AAL legend applied to plots", type = "message", duration = 2)
          } else if (grp == "pct") {
            pct_scheme_committed(pct_scheme_staging())
            shiny::showNotification("Percentage Change legend applied to plots", type = "message", duration = 2)
          }
          
          session$sendCustomMessage("sync-colour-swatches", list())
          shiny::showNotification(paste("Applied defaults to all", grp, "plots"), type = "message", duration = 2)
        })
      })
    }
    
    # -------------------------------------------------------------------------
    # Gallery-level controls (Legend Configuration UI)
    # -------------------------------------------------------------------------
    output$region_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$n_region > 0)
      def <- defaults[["region"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "region", n = rv$n_region,
        title = "Regionwise - Gallery defaults",
        default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
        default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
        default_top_margin_px = def$top_margin_px, default_bottom_margin_px = def$bottom_margin_px,
        default_left_margin_px = def$left_margin_px, default_right_margin_px = def$right_margin_px,
        default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour,
        has_facets = TRUE,
        legend_ui = vul_legend_config_ui(session$ns, "rel", "Legend Configuration - Relative AAL", editable = TRUE)
      )
    })
    
    output$state_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$n_state > 0)
      def <- defaults[["state"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "state", n = rv$n_state,
        title = "Statewise - Gallery defaults",
        default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
        default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
        default_top_margin_px = def$top_margin_px, default_bottom_margin_px = def$bottom_margin_px,
        default_left_margin_px = def$left_margin_px, default_right_margin_px = def$right_margin_px,
        default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour,
        has_facets = TRUE,
        legend_ui = vul_legend_config_ui(
          session$ns, "rel", "Legend Configuration - Relative AAL", editable = FALSE,
          readonly_note = "This legend is shared with the Regionwise tab. Edit it there - changes apply to both."
        )
      )
    })
    
    output$pct_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$n_pct > 0)
      def <- defaults[["pct"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "pct", n = rv$n_pct,
        title = "Percentage Change - Gallery defaults",
        default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
        default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
        default_top_margin_px = def$top_margin_px, default_bottom_margin_px = def$bottom_margin_px,
        default_left_margin_px = def$left_margin_px, default_right_margin_px = def$right_margin_px,
        default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
        default_plot_title = def$plot_title,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour,
        has_facets = FALSE,
        legend_ui = vul_legend_config_ui(session$ns, "pct", "Legend Configuration - Percentage Change", editable = TRUE)
      )
    })
    
    # -------------------------------------------------------------------------
    # Plot galleries (per-plot cards)
    # -------------------------------------------------------------------------
    output$region_plots_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      shiny::req(rv$plots_generated, rv$plots)
      
      p <- rv$plots
      rnames <- intersect(PLOT_ORDER, names(p$region))
      if (length(rnames) == 0) {
        return(div(
          class = "info-panel", style = "margin-top: 20px; padding: 30px; text-align: center;",
          h4("No regionwise plots could be generated"),
          p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
          p("Check the R console for diagnostic messages showing the unique Classification values."),
          p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
        ))
      }
      
      shiny::tagList(
        shiny::h2("Regionwise Comparison"),
        lapply(rnames, function(nm) {
          def <- defaults[["region"]]()
          vul_plot_card_gallery(
            ns = session$ns, key = nm, label = nm, prefix = "region",
            default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
            default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
            default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour,
            has_facets = TRUE
          )
        })
      )
    })
    
    output$state_plots_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      shiny::req(rv$plots_generated, rv$plots)
      
      p <- rv$plots
      snames <- intersect(PLOT_ORDER, names(p$state))
      if (length(snames) == 0) {
        return(div(
          class = "info-panel", style = "margin-top: 20px; padding: 30px; text-align: center;",
          h4("No statewise plots could be generated"),
          p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
          p("Check the R console for diagnostic messages showing the unique Classification values."),
          p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
        ))
      }
      
      shiny::tagList(
        shiny::h2("Statewise Comparison"),
        lapply(snames, function(nm) {
          def <- defaults[["state"]]()
          vul_plot_card_gallery(
            ns = session$ns, key = nm, label = nm, prefix = "state",
            default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
            default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
            default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour,
            has_facets = TRUE
          )
        })
      )
    })
    
    output$pct_plots_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      shiny::req(rv$plots_generated, rv$plots)
      
      p <- rv$plots
      pnames <- intersect(PLOT_ORDER, names(p$pct))
      if (length(pnames) == 0) {
        return(div(
          class = "info-panel", style = "margin-top: 20px; padding: 30px; text-align: center;",
          h4("No percentage-change plots could be generated"),
          p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
          p("Check the R console for diagnostic messages showing the unique Classification values."),
          p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
        ))
      }
      
      shiny::tagList(
        shiny::h2("Percentage Change Comparison"),
        lapply(pnames, function(nm) {
          def <- defaults[["pct"]]()
          vul_plot_card_gallery(
            ns = session$ns, key = nm, label = nm, prefix = "pct",
            default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
            default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
            default_axis_text = def$axis_text, default_x_rotation = def$x_rotation, default_x_vjust = def$x_vjust,
            default_plot_title = def$plot_title,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour,
            has_facets = FALSE
          )
        })
      )
    })
    
    # -------------------------------------------------------------------------
    # Processed table with clean column names (Problem 2) and pagination (Problem 5)
    # -------------------------------------------------------------------------
    output$processed_table <- DT::renderDT({
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      df <- st$obj$final_comp
      
      # Rename columns for display (Problem 2) – rename the data frame itself
      colnames_map <- c(
        LOCNUM     = "Locnum",
        LOCNAME    = "Locname",
        STATECODE  = "Statecode",
        COUNTY     = "County",
        key        = "Key",
        model_new  = "Model New",
        model_old  = "Model Old",
        model_1    = "Model-1",
        model_2    = "Model-2",
        model_title = "Model Title",
        UniqueID   = "UniqueID"
      )
      # Only rename columns that exist
      existing <- intersect(names(df), names(colnames_map))
      # Apply renaming
      names(df)[match(existing, names(df))] <- colnames_map[existing]
      
      DT::datatable(
        df,
        options = list(
          pageLength = 10,        # Problem 5: 10 rows per page
          scrollX = TRUE
        ),
        rownames = FALSE,
        filter = "top"
      )
    })
    
    # -------------------------------------------------------------------------
    # Processed metadata display (Problem 5)
    # -------------------------------------------------------------------------
    output$processed_metadata <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      o <- st$obj
      df <- o$final_comp
      
      file_name <- if (!is.null(input$file)) {
        input$file$name
      } else if (isTRUE(sample_loaded())) {
        if (identical(input$model_family, "moody")) "Moody's sample" else "Verisk sample"
      } else {
        "No file loaded"
      }
      
      div(
        class = "info-panel",
        style = "margin-bottom: 12px; padding: 12px 16px;",
        div(
          style = "display: flex; flex-wrap: wrap; gap: 20px;",
          div(style = "font-weight: 600;", "File:", file_name),
          div(style = "font-weight: 600;", "Rows:", format(nrow(df), big.mark = ",")),
          div(style = "font-weight: 600;", "States:", length(unique(df$STATECODE)))
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Sample downloads
    # -------------------------------------------------------------------------
    output$download_sample_moody <- shiny::downloadHandler(
      filename = function() basename(sample_moody_path),
      content = function(file) {
        if (!file.exists(sample_moody_path)) stop("Moody's sample file not found: ", sample_moody_path)
        file.copy(sample_moody_path, file, overwrite = TRUE)
      }
    )
    
    output$download_sample_verisk <- shiny::downloadHandler(
      filename = function() basename(sample_verisk_path),
      content = function(file) {
        if (!file.exists(sample_verisk_path)) stop("Verisk sample file not found: ", sample_verisk_path)
        file.copy(sample_verisk_path, file, overwrite = TRUE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Main download handler
    # -------------------------------------------------------------------------
    output$download_selected <- shiny::downloadHandler(
      filename = function() {
        choice <- input$download_type %||% "data"
        ext <- switch(choice, "html" = ".html", "html_customized" = ".html", "plots" = ".zip", "data" = ".csv", "all" = ".zip", ".zip")
        paste0("vulsen_output_", Sys.Date(), ext)
      },
      content = function(file) {
        st <- obj_state()
        shiny::req(isTRUE(st$ok), st$obj)
        obj <- st$obj
        plots <- rv$plots
        choice <- input$download_type %||% "data"
        rv$downloaded <- TRUE
        
        if (choice == "data") {
          utils::write.csv(obj$final_comp, file, row.names = FALSE)
          return(invisible(file))
        }
        
        if (choice == "html") {
          can_use_template <- exists("save_html_report", mode = "function") && file.exists(file.path("templates", "vulsens_report.Rmd"))
          if (can_use_template) save_html_report(obj, file) else save_vulsen_fallback_html(obj, plots, file)
          return(invisible(file))
        }
        
        # -----------------------------------------------------------------
        # build_customized_html_bundles()
        #
        # Rebuilds every plot in every group with vul_apply_overrides() using
        # each card's *current* input values - the same function/inputs the
        # on-screen renderPlot(), the "All Plots" zip, and Add-to-Cart all
        # use - so what's exported here is pixel-identical to what's on
        # screen, with every customisation (size, colours, text, legend,
        # labels) preserved. Unlike the plain "HTML Report" option above,
        # nothing here falls back to a re-derived default.
        # -----------------------------------------------------------------
        build_customized_html_bundles <- function() {
          out <- list(region = list(), state = list(), pct = list())
          for (group in c("region", "state", "pct")) {
            plot_list <- plots[[group]]
            if (is.null(plot_list) || !length(plot_list)) next
            def <- defaults[[group]]()
            for (key in names(plot_list)) {
              safe_key  <- sec_safe_id_key(key)
              base_plot <- plot_list[[key]]
              p <- vul_apply_overrides(base_plot, input, group, safe_key, def)
              out[[group]][[key]] <- list(
                plot   = p,
                width  = attr(p, "vulsen_width_in")  %||% def$width_in  %||% 9,
                height = attr(p, "vulsen_height_in") %||% def$height_in %||% 5,
                dpi    = attr(p, "vulsen_dpi")       %||% def$dpi       %||% 300,
                bg     = attr(p, "vulsen_bg")        %||% "white",
                label  = key
              )
            }
          }
          out
        }
        
        render_customized_html <- function(out_file) {
          template_path <- file.path("R", "VulsenAPP_utils", "vulnerability_customized_template.Rmd")
          if (!file.exists(template_path)) {
            stop("Customized template not found: ", template_path)
          }
          bundles <- build_customized_html_bundles()
          rmarkdown::render(
            input         = template_path,
            output_file   = out_file,
            params = list(
              region_plots = bundles$region,
              state_plots  = bundles$state,
              pct_plots    = bundles$pct,
              model_title  = obj$model_title,
              input_rows   = nrow(obj$final_comp),
              n_states     = length(unique(obj$final_comp$STATECODE)),
              n_regions    = length(unique(obj$final_comp$Region))
            ),
            knit_root_dir = getwd(),
            envir         = new.env(parent = globalenv()),
            quiet         = TRUE
          )
          invisible(out_file)
        }
        
        if (choice == "html_customized") {
          render_customized_html(file)
          return(invisible(file))
        }
        
        tmp_dir <- tempfile("vulsen_download_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        
        plot_dir <- file.path(tmp_dir, "plots")
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
        
        plot_files <- c()
        for (group in c("region", "state", "pct")) {
          plot_list <- plots[[group]]
          if (is.null(plot_list)) next
          for (key in names(plot_list)) {
            safe_key <- sec_safe_id_key(key)
            base_plot <- plot_list[[key]]
            def <- defaults[[group]]()
            p <- vul_apply_overrides(base_plot, input, group, safe_key, def)
            w   <- attr(p, "vulsen_width_in")  %||% def$width_in  %||% 9
            h   <- attr(p, "vulsen_height_in") %||% def$height_in %||% 5
            dpi <- attr(p, "vulsen_dpi")       %||% def$dpi       %||% 300
            bg  <- attr(p, "vulsen_bg")        %||% "white"
            fname <- paste0(group, "_", safe_key, ".png")
            ggplot2::ggsave(file.path(plot_dir, fname), plot = p, width = w, height = h, dpi = dpi, bg = bg, limitsize = FALSE)
            plot_files <- c(plot_files, file.path(plot_dir, fname))
          }
        }
        
        files_to_zip <- plot_files
        
        if (choice == "all") {
          data_file <- file.path(tmp_dir, "processed_data.csv")
          utils::write.csv(obj$final_comp, data_file, row.names = FALSE)
          files_to_zip <- c(files_to_zip, data_file)
          
          html_file <- file.path(tmp_dir, "vulnerability_sensitivity_report.html")
          save_vulsen_fallback_html(obj, plots, html_file)
          files_to_zip <- c(files_to_zip, html_file)
          
          html_customized_file <- file.path(tmp_dir, "vulnerability_sensitivity_report_customized.html")
          tryCatch({
            render_customized_html(html_customized_file)
            files_to_zip <- c(files_to_zip, html_customized_file)
          }, error = function(e) {
            showNotification(paste("Customized HTML skipped in bundle:", conditionMessage(e)), type = "warning", duration = 6)
          })
          
          rel_json <- file.path(tmp_dir, "relative_aal_legend.json")
          writeLines(as.character(vul_scheme_to_json(rel_scheme_committed(), "relative_aal_scheme", "relative_aal")), rel_json)
          files_to_zip <- c(files_to_zip, rel_json)
          
          pct_json <- file.path(tmp_dir, "percentage_change_legend.json")
          writeLines(as.character(vul_scheme_to_json(pct_scheme_committed(), "percentage_change_scheme", "percentage_change")), pct_json)
          files_to_zip <- c(files_to_zip, pct_json)
        }
        
        if (!length(files_to_zip)) {
          empty_note <- file.path(tmp_dir, "README.txt")
          writeLines("No plots were available to export.", empty_note)
          files_to_zip <- empty_note
        }
        
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(tmp_dir)
        
        rel_files <- gsub(
          pattern = paste0("^", normalizePath(tmp_dir, winslash = "/", mustWork = TRUE), "/?"),
          replacement = "",
          x = normalizePath(files_to_zip, winslash = "/", mustWork = TRUE)
        )
        
        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zipr(zipfile = file, files = rel_files)
        } else {
          utils::zip(zipfile = file, files = rel_files)
        }
        
        invisible(file)
      }
    )
    
    # -------------------------------------------------------------------------
    # Fallback HTML report function (if template missing)
    # -------------------------------------------------------------------------
    save_vulsen_fallback_html <- function(obj, plots, file) {
      tmp_dir <- tempfile("vulsen_html_assets_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      
      save_plot_list <- function(plot_list, section_name) {
        out <- list()
        if (is.null(plot_list) || !length(plot_list)) return(out)
        for (nm in names(plot_list)) {
          safe_name <- gsub("[^A-Za-z0-9_]+", "_", paste(section_name, nm, sep = "_"))
          img_file <- file.path(tmp_dir, paste0(safe_name, ".png"))
          ggplot2::ggsave(img_file, plot = plot_list[[nm]], width = 14, height = 8, dpi = 300, limitsize = FALSE)
          out[[length(out) + 1]] <- list(section = section_name, title = nm, path = img_file)
        }
        out
      }
      
      img_records <- c(
        save_plot_list(plots$region, "Regionwise Comparison"),
        save_plot_list(plots$state,  "Statewise Comparison"),
        save_plot_list(plots$pct,    "Percentage Change Comparison")
      )
      
      img_tags <- lapply(img_records, function(x) {
        htmltools::tags$div(
          class = "plot-card",
          htmltools::tags$h3(paste0(x$section, " - ", x$title)),
          htmltools::tags$img(src = knitr::image_uri(x$path), style = "max-width:100%; height:auto; border:1px solid #d8dce6;")
        )
      })
      
      body <- htmltools::tagList(
        htmltools::tags$html(
          htmltools::tags$head(
            htmltools::tags$meta(charset = "utf-8"),
            htmltools::tags$title("Vulnerability Sensitivity Report"),
            htmltools::tags$style(htmltools::HTML("
                body { font-family: Arial, sans-serif; margin: 24px; background: #f6f8fb; color: #1A1A2E; }
                .masthead { background: #6FACDE; padding: 22px 26px; border-radius: 16px; margin-bottom: 20px; color: #0F1B3D; }
                .masthead h1 { margin: 0 0 6px 0; font-size: 28px; }
                .masthead p { margin: 0; font-size: 14px; }
                .metric-row { display: flex; gap: 12px; margin-bottom: 18px; flex-wrap: wrap; }
                .metric-card { background: white; border-radius: 12px; padding: 14px 18px; box-shadow: 0 4px 14px rgba(0,0,0,0.08); min-width: 160px; }
                .metric-value { font-size: 22px; font-weight: 700; color: #0075BC; }
                .metric-label { font-size: 12px; color: #666; margin-top: 4px; }
                .plot-card { background: white; border-radius: 14px; padding: 16px; margin-bottom: 22px; box-shadow: 0 4px 14px rgba(0,0,0,0.08); }
                .plot-card h3 { margin-top: 0; color: #0F1B3D; }
              "))
          ),
          htmltools::tags$body(
            htmltools::tags$div(
              class = "masthead",
              htmltools::tags$h1("Vulnerability Sensitivity Report"),
              htmltools::tags$p(paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Model family: ", obj$model_title))
            ),
            htmltools::tags$div(
              class = "metric-row",
              htmltools::tags$div(class = "metric-card", htmltools::tags$div(class = "metric-value", format(nrow(obj$final_comp), big.mark = ",")), htmltools::tags$div(class = "metric-label", "Input rows")),
              htmltools::tags$div(class = "metric-card", htmltools::tags$div(class = "metric-value", length(unique(obj$final_comp$STATECODE))), htmltools::tags$div(class = "metric-label", "States")),
              htmltools::tags$div(class = "metric-card", htmltools::tags$div(class = "metric-value", length(unique(obj$final_comp$Region))), htmltools::tags$div(class = "metric-label", "Regions"))
            ),
            img_tags
          )
        )
      )
      
      htmltools::save_html(body, file = file)
      invisible(file)
    }
    
  })
}

