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
    
    # ---- Fallback for standalone mode (if parent app doesn't provide it) ----
    if (!exists("generate_item_id", mode = "function")) {
      generate_item_id <- function() {
        paste0("item_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
      }
    }
    
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
      header_error_msg = "",
      region_available = FALSE
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
    
    # ---- Helper: wire up one legend tab ----
    setup_legend_manager <- function(tag, scheme_staging, committed_scheme, default_bins, scheme_type, value_source) {
      
      output[[paste0(tag, "_legend_table")]] <- DT::renderDT({
        vul_legend_datatable(scheme_staging())
      }, server = FALSE)
      
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
        
        if (col == "lower" && row < nrow(df)) {
          df[row + 1, "upper"] <- val
        } else if (col == "upper" && row > 1) {
          df[row - 1, "lower"] <- val
        }
        
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
      
      shiny::observeEvent(input[[paste0(tag, "_legend_load_default")]], {
        scheme_staging(default_bins)
        legend_msg[[tag]](NULL)
        shiny::showNotification("Default legend loaded (apply to plots to see changes)", type = "message", duration = 2)
      })
      
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
      
      output[[paste0(tag, "_legend_download_json")]] <- shiny::downloadHandler(
        filename = function() paste0("vulsen_", tag, "_legend_", Sys.Date(), ".json"),
        content = function(file) {
          json <- vul_scheme_to_json(scheme_staging(), paste0(tag, "_scheme"), scheme_type)
          writeLines(as.character(json), file)
        }
      )
      
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
      
      output[[paste0(tag, "_legend_msg")]] <- shiny::renderUI({
        msg <- legend_msg[[tag]]()
        if (is.null(msg)) return(NULL)
        status_box("error", msg)
      })
      
      output[[paste0(tag, "_legend_preview")]] <- shiny::renderUI({
        vul_legend_preview_tags(scheme_staging())
      })
      
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
    # X-Axis Class Order & Labels - modal-based editor (v2)
    #
    # v1 rendered the Order/Name/Rename table inline (DT::renderDT) inside
    # each plot card's collapsible override panel. That panel is a CSS grid
    # that starts display:none, and DataTables' width calculation doesn't
    # handle that reliably - columns collapse to 0px and never repaint until
    # something forces a browser reflow (confirmed via DevTools).
    #
    # v2 moves the whole editor into a modalDialog and drops DT for this
    # feature entirely:
    #   - class_order_committed[[ck]] is the ONLY server-side reactive state
    #     now - it's what vul_apply_overrides() reads at every call site,
    #     completely unchanged from before.
    #   - Reordering (arrows) and renaming (text inputs) happen ENTIRELY in
    #     JS inside the modal, against a plain in-memory JS array - zero
    #     Shiny.setInputValue calls, zero server round-trips, per click or
    #     keystroke. That's what makes it feel instant instead of triggering
    #     a table/plot re-render on every change.
    #   - The modal's Apply button is the ONLY point that talks back to
    #     Shiny: it fires exactly one Shiny.setInputValue() carrying the
    #     full final order + labels as a JSON string. The observer below is
    #     the only place class_order_committed[[ck]] changes, and that one
    #     change is what re-triggers the existing plot pipeline + closes
    #     the modal.
    #   - class_order_staging (used only for the old live server-side DT
    #     edits) is gone - nothing downstream ever referenced it.
    #
    # class_order_bound_env is unchanged from v1: a plain (non-reactive)
    # environment guarding against re-registering the open/apply
    # observeEvent's for the same card - the outer render loop that calls
    # setup_class_order_manager() re-runs every time plots are
    # (re)generated, and without this guard every regeneration would stack
    # a new, duplicate observer on top of the old ones.
    # -------------------------------------------------------------------------
    class_order_committed <- shiny::reactiveValues()
    class_order_bound_env <- new.env(parent = emptyenv())
    
    #' Wire up the "X-Axis Class Order & Labels" open-modal button and its
    #' Apply handler for one plot card. Called once per card, every time
    #' the card is (re)built by the render loop - guarded so repeated
    #' calls don't stack duplicate observers.
    #'
    #' @param prefix "region" | "state" | "pct"
    #' @param key The plot's Classification label (e.g. "CC") - same value
    #'   already used to index ORDER_LIST / default_class_order_state().
    #' @param safe_key sec_safe_id_key(key) - same sanitized id fragment
    #'   already used for every other per-card input/output id.
    setup_class_order_manager <- function(prefix, key, safe_key) {
      ck <- paste0(prefix, "_", safe_key)
      
      if (is.null(class_order_committed[[ck]])) {
        class_order_committed[[ck]] <- default_class_order_state(key)
      }
      
      open_id  <- paste0(prefix, "_class_order_open_", safe_key)
      apply_id <- paste0(prefix, "_class_order_apply_", safe_key)
      
      # ---- Register once per card, ever ----
      if (isTRUE(class_order_bound_env[[ck]])) return(invisible(NULL))
      class_order_bound_env[[ck]] <- TRUE
      
      # ---- Open modal, seeded from the current committed state ----
      shiny::observeEvent(input[[open_id]], {
        shiny::showModal(
          vul_class_order_modal_ui(
            ns             = session$ns,
            key            = key,
            apply_input_id = apply_id,
            order_df       = class_order_committed[[ck]]
          )
        )
      })
      
      # ---- Apply: one JSON payload from the modal's JS, one commit ----
      shiny::observeEvent(input[[apply_id]], {
        payload <- input[[apply_id]]
        shiny::req(payload)
        
        parsed <- tryCatch(
          jsonlite::fromJSON(payload, simplifyDataFrame = TRUE),
          error = function(e) NULL
        )
        
        if (is.null(parsed) || !NROW(parsed)) {
          shiny::showNotification("Could not apply class order - malformed data.", type = "error", duration = 3)
          return(invisible(NULL))
        }
        
        # `visible` is new (per-class Show/Hide toggle). Older seeds / a
        # modal JS bundle that hasn't been refreshed yet won't send it, so
        # default missing/blank values to TRUE (visible) rather than
        # erroring - keeps this backward compatible with any in-flight
        # payload shape.
        visible_raw <- parsed$visible
        if (is.null(visible_raw)) visible_raw <- rep(TRUE, NROW(parsed))
        
        df <- data.frame(
          order   = as.integer(parsed$order),
          name    = as.character(parsed$name),
          rename  = as.character(parsed$rename),
          visible = as.logical(visible_raw),
          stringsAsFactors = FALSE
        )
        df$visible[is.na(df$visible)] <- TRUE
        
        # Sanity-check the payload against what the modal was seeded with -
        # same class set, same row count, no blank renames, and at least
        # one class left visible (an all-hidden axis would render an empty
        # plot) - before trusting it as the new committed state.
        seed <- class_order_committed[[ck]]
        if (nrow(df) != nrow(seed) || !setequal(df$name, seed$name) ||
            anyNA(df$order) || any(is.na(df$rename) | !nzchar(trimws(df$rename)))) {
          shiny::showNotification("Could not apply class order - unexpected or incomplete data.", type = "error", duration = 3)
          return(invisible(NULL))
        }
        if (!any(df$visible)) {
          shiny::showNotification("At least one class must stay visible.", type = "error", duration = 3)
          return(invisible(NULL))
        }
        
        class_order_committed[[ck]] <- df
        shiny::removeModal()
        shiny::showNotification("Class order & labels applied to this plot", type = "message", duration = 2.5)
      })
    }
    
    # -------------------------------------------------------------------------
    # Sample data paths
    # -------------------------------------------------------------------------
    sample_moody_path <- file.path("data", "VulsenAPP_sample", "USSCS_RMS_HDv1_DLM_AAL_AllPeril_vulsen_comp_CCMasonary_SAMPLE.csv")
    sample_verisk_path <- file.path("data", "VulsenAPP_sample", "USSCS_Verisk_v12v13_vulsen_comp_AllPeril_SAMPLE.csv")
    sample_region_path <- file.path("data", "VulsenAPP_sample", "region.csv")
    
    # -------------------------------------------------------------------------
    # RAW DATA LOADING
    # -------------------------------------------------------------------------
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
      # No data loaded yet
      list(ok = FALSE, msg = "No data loaded", data = NULL)
    })
    
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
    # Same class of bug as subperil_ui below (see the note there): this
    # output must not silently stay suspended depending on what else was
    # happening in the reactive flush at the moment the user navigated
    # back to this tab. Force it to always recompute when its inputs
    # change, regardless of visibility/binding timing.
    shiny::outputOptions(output, "model_columns_ui", suspendWhenHidden = FALSE)
    
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
    # Show/hide the Percentage Change tab (initially, but will be overridden)
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
    sample_region_loaded <- shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(input$load_sample, {
      sample_loaded(TRUE)
      shiny::showNotification("Sample data loaded.", type = "message", duration = 4)
    })
    
    shiny::observeEvent(input$load_sample_region, {
      sample_region_loaded(TRUE)
      shiny::showNotification("Sample region mapping loaded.", type = "message", duration = 4)
    })
    
    # -------------------------------------------------------------------------
    # File upload notification
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      shiny::showNotification("File uploaded successfully.", type = "message", duration = 4)
    })
    
    # ---- Region mapping data ----
    region_map_data <- shiny::reactive({
      if (!is.null(input$region_map_file)) {
        # User uploaded a file
        tryCatch({
          df <- data.table::fread(input$region_map_file$datapath, data.table = FALSE)
          # Normalise column names: look for statecode and region
          names(df) <- tolower(trimws(names(df)))
          # Find key and region columns
          key_col <- grep("^state|^province|^code|^region_code", names(df), value = TRUE)[1]
          region_col <- grep("^region|^area|^group", names(df), value = TRUE)[1]
          if (is.na(key_col) || is.na(region_col)) {
            # Fallback: use first two columns
            key_col <- names(df)[1]
            region_col <- names(df)[2]
          }
          # Rename to standard
          df_out <- data.frame(STATECODE = as.character(df[[key_col]]),
                               Region = as.character(df[[region_col]]),
                               stringsAsFactors = FALSE)
          df_out <- df_out[!is.na(df_out$STATECODE) & !is.na(df_out$Region), ]
          if (nrow(df_out) == 0) {
            shiny::showNotification("Region mapping file has no valid rows.", type = "warning")
            return(NULL)
          }
          return(df_out)
        }, error = function(e) {
          shiny::showNotification(paste("Error reading region mapping file:", e$message), type = "error")
          return(NULL)
        })
      } else if (isTRUE(sample_region_loaded())) {
        # Load sample region mapping
        if (file.exists(sample_region_path)) {
          df <- data.table::fread(sample_region_path, data.table = FALSE)
          names(df) <- tolower(trimws(names(df)))
          key_col <- grep("^state|^province|^code", names(df), value = TRUE)[1]
          region_col <- grep("^region", names(df), value = TRUE)[1]
          if (is.na(key_col) || is.na(region_col)) {
            key_col <- names(df)[1]
            region_col <- names(df)[2]
          }
          df_out <- data.frame(STATECODE = as.character(df[[key_col]]),
                               Region = as.character(df[[region_col]]),
                               stringsAsFactors = FALSE)
          df_out <- df_out[!is.na(df_out$STATECODE) & !is.na(df_out$Region), ]
          if (nrow(df_out) == 0) {
            shiny::showNotification("Sample region mapping has no valid rows.", type = "warning")
            return(NULL)
          }
          return(df_out)
        } else {
          shiny::showNotification("Sample region mapping file not found.", type = "error")
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    
    # -------------------------------------------------------------------------
    # Header validation (flexible, case-insensitive)
    # -------------------------------------------------------------------------
    shiny::observe({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        rv$header_error <- FALSE
        rv$header_error_msg <- ""
        return()
      }
      val <- validate_csv_headers(rs$data, input$model_family, input$model1_col,
                                  if (identical(input$n_models, "2")) input$model2_col else NULL)
      if (val$ok) {
        rv$header_error <- FALSE
        rv$header_error_msg <- ""
      } else {
        rv$header_error <- TRUE
        rv$header_error_msg <- val$msg
        shiny::showModal(
          shiny::modalDialog(
            title = "Invalid CSV Headers",
            shiny::tags$pre(style = "white-space: pre-wrap;", val$msg),
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          )
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Dynamic subperil update
    # -------------------------------------------------------------------------
    # This used to be observeEvent(input$peril, { updateSelectInput(...) }),
    # which only fires when input$peril's VALUE changes. The parent app
    # rebuilds this whole module's UI from scratch every time the user
    # navigates back to this tab (output$app_ui <- renderUI({ switch(active_page(), ...) })),
    # and the freshly-inserted "peril" select re-sends its default ("SCS")
    # on every visit. If the user was already on "SCS" from a prior visit,
    # that's not a change, so the old observer never re-fired and the
    # freshly-recreated "subperil" select (born with choices = NULL) stayed
    # empty on the 2nd+ visit.
    #
    # A renderUI-backed output doesn't have that problem: when its bound
    # uiOutput() container is torn down and a new one with the same id is
    # inserted, Shiny suspends and then resumes the output, forcing it to
    # recompute regardless of whether input$peril actually changed. It also
    # still reacts normally to the user changing Peril within a single visit.
    #
    # NOTE: deliberately does NOT try to "preserve" input$subperil across a
    # remount. On a whole-page teardown/rebuild, input$subperil can still
    # hold a stale value from the previous visit for a brief window before
    # the freshly-inserted <select> re-binds and reports its own default —
    # reading it here to decide `selected` risked selecting a value that
    # doesn't actually match the new DOM element's real state. Always
    # deriving `selected` fresh from `choices` (mirroring the known-working
    # input_preparation_server.R version) removes that race entirely.
    #
    # REQUIRES a matching one-line change in ui/VulSen_ui.R: swap
    #   selectInput(ns("subperil"), "Subperil", choices = NULL, selected = "AllPeril", width = "100%")
    # for
    #   shiny::uiOutput(ns("subperil_ui"))
    #
    # ADDENDUM: the suspend/resume-on-rebind mechanism above still depends on
    # the client's "this output is visible again" signal arriving and being
    # processed before anything else invalidates the reactive graph. In
    # practice that ordering is sensitive to how much other reactive/DOM
    # work is in flight at the moment of navigation (e.g. arriving here via
    # Result Extraction after doing work there, vs. a clean visit straight
    # from Home) -- confirmed by reproducing a case where this output's
    # bound <div> stayed completely empty (no content, no error) after such
    # a navigation. outputOptions(..., suspendWhenHidden = FALSE) below
    # removes the dependency on that timing entirely: this output always
    # recomputes when input$peril changes, whether or not it's currently
    # considered "visible". Same fix already used for the legend table/
    # preview outputs above (see setup_legend_manager()).
    # -------------------------------------------------------------------------
    output$subperil_ui <- shiny::renderUI({
      peril_val <- input$peril %||% "SCS"
      choices <- peril_lookup()[[peril_val]]
      if (is.null(choices)) choices <- character(0)
      selected <- if ("AllPeril" %in% choices) "AllPeril" else choices[1]
      shiny::selectInput(ns("subperil"), "Subperil", choices = choices,
                          selected = selected, width = "100%", selectize = FALSE)
    })
    shiny::outputOptions(output, "subperil_ui", suspendWhenHidden = FALSE)
    
    # -------------------------------------------------------------------------
    # Build analysis object (with optional region mapping join)
    # -------------------------------------------------------------------------
    obj_state <- shiny::reactive({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        return(list(ok = FALSE, msg = rs$msg, obj = NULL))
      }
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
        
        final_comp <- standardize_vulsens(rs$data, input$model_family, input$model1_col, model2_col)
        apply_class_config(build_class_config(final_comp))   # <-- new line: refresh CLASS_LABELS/PLOT_ORDER/ORDER_LIST
        o <- build_rmd_objects(rs$data, input$model_family, input$model1_col, model2_col, meta)
        
        
        # ---- Optional region mapping join ----
        if (!("Region" %in% names(o$final_comp)) || all(is.na(o$final_comp$Region))) {
          map <- region_map_data()
          if (!is.null(map) && nrow(map) > 0) {
            # Left join on STATECODE
            o$final_comp <- o$final_comp |>
              dplyr::left_join(map, by = "STATECODE") |>
              dplyr::mutate(Region = dplyr::coalesce(Region.x, Region.y)) |>
              dplyr::select(-Region.x, -Region.y)
            # Rebuild region splits if Region was added
            if ("Region" %in% names(o$final_comp) && any(!is.na(o$final_comp$Region))) {
              avg_region <- summarise_average(o$final_comp, TRUE, o$n_models)
              region_obj <- unk_vulfile(avg_region, "Region", o$n_models)
              o$region_split <- split_by_class(region_obj$unk_comp)
              o$avg_region <- avg_region
            }
          }
        }
        
        # Check if Region is available
        has_region <- "Region" %in% names(o$final_comp) && any(!is.na(o$final_comp$Region))
        rv$region_available <- has_region
        
        # Update tab visibility
        if (has_region) {
          shiny::showTab(inputId = "vulsen_tabs", target = "region_tab")
          if (identical(n_models, "2")) {
            shiny::showTab(inputId = "vulsen_tabs", target = "pct_tab")
          }
        } else {
          shiny::hideTab(inputId = "vulsen_tabs", target = "region_tab")
          shiny::hideTab(inputId = "vulsen_tabs", target = "pct_tab")
        }
        
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
    # Metrics UI
    # -------------------------------------------------------------------------
    output$metrics_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      o <- st$obj
      shiny::fluidRow(
        shiny::column(3, metric_card(format(nrow(o$final_comp), big.mark = ","), "Input rows")),
        shiny::column(3, metric_card(length(unique(o$final_comp$STATECODE)), "States")),
        shiny::column(3, metric_card(length(unique(o$final_comp$Region[!is.na(o$final_comp$Region)])), "Regions")),
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
      
      # ---- Auto-open the appropriate tab based on which plots are available ----
      if (length(plots$region) > 0) {
        shiny::updateTabsetPanel(session, "vulsen_tabs", selected = "region_tab")
      } else if (length(plots$state) > 0) {
        shiny::updateTabsetPanel(session, "vulsen_tabs", selected = "state_tab")
      } else if (length(plots$pct) > 0) {
        shiny::updateTabsetPanel(session, "vulsen_tabs", selected = "pct_tab")
      }
      
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
      shiny::showNotification("Plots generated.", type = "message", duration = 4)
      
      # Render plot outputs
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
              
              # X-Axis Class Order & Labels - table/preview render + move/
              # rename/Apply observers for this card (safe to call every
              # regeneration - see setup_class_order_manager()'s own guard).
              setup_class_order_manager(prefix, key, safe_key)
              
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
              
              output[[paste0(prefix, "_plot_", safe_key)]] <- shiny::renderPlot({
                base_plot <- rv$plots[[prefix]][[key]]
                shiny::req(base_plot)
                def <- defaults[[prefix]]()
                p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def,
                                         class_order = class_order_committed[[paste0(prefix, "_", safe_key)]])
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
              
              output[[paste0(prefix, "_dl_", safe_key)]] <- shiny::downloadHandler(
                filename = function() paste0("vulsen_", prefix, "_", safe_key, "_", Sys.Date(), ".png"),
                content = function(file) {
                  base_plot <- rv$plots[[prefix]][[key]]
                  shiny::req(base_plot)
                  def <- defaults[[prefix]]()
                  p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def,
                                           class_order = class_order_committed[[paste0(prefix, "_", safe_key)]])
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
      p <- vul_apply_overrides(base_plot, input, prefix, safe_key, def,
                               class_order = class_order_committed[[paste0(prefix, "_", safe_key)]])
      
      w   <- attr(p, "vulsen_width_in")  %||% def$width_in  %||% 9
      h   <- attr(p, "vulsen_height_in") %||% def$height_in %||% 5
      dpi <- attr(p, "vulsen_dpi")       %||% def$dpi       %||% 300
      
      # ---- Class/bin colour legend active for this plot ----
      # Unlike Secondary Modifier (which always has exactly 4 fixed named
      # colours: SFD/COM/Penalty/Credit), VulSen's legend colours come from
      # a variable-length bin table (rel_scheme_committed() for
      # region/state plots, pct_scheme_committed() for pct plots), each
      # row holding a level/label/lower/upper/colour. We flatten that into
      # a single named character vector (names = class labels, values =
      # hex codes) so docx_generator.R can print one hex per class the
      # same way it already prints SFD/COM/Penalty/Credit.
      colour_scheme <- if (identical(prefix, "pct")) pct_scheme_committed() else rel_scheme_committed()
      class_colours <- NULL
      if (!is.null(colour_scheme) && NROW(colour_scheme) > 0 &&
          all(c("level", "label", "colour") %in% names(colour_scheme))) {
        cs <- colour_scheme[order(-suppressWarnings(as.numeric(colour_scheme$level))), c("label", "colour")]
        class_colours <- stats::setNames(as.character(cs$colour), as.character(cs$label))
      }
      
      # Attach the styling/defaults active at add-to-cart time, plus the
      # actual dimensions used and the class colour legend, so
      # docx_generator.R can render a per-figure settings block (including
      # per-class hex codes) in the Word report.
      customization <- c(def, list(width_in = w, height_in = h, dpi = dpi,
                                    class_colours = class_colours))
      
      item <- list(
        id            = generate_item_id(),
        module        = "Vulnerability",
        plot          = p,
        commentary    = NULL,
        timestamp     = Sys.time(),
        width         = w,
        height        = h,
        dpi           = dpi,
        customization = customization
      )
      
      current_cart <- cart()
      current_cart[[length(current_cart) + 1]] <- item
      cart(current_cart)
      
      shiny::showNotification("Added to cart", type = "message", duration = 2)
    })
    
    # -------------------------------------------------------------------------
    # "Apply to all" observers
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
    shiny::outputOptions(output, "region_gallery_controls", suspendWhenHidden = FALSE)
    
    output$state_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$n_state > 0)
      def <- defaults[["state"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "state", n = rv$n_state,
        title = "Statewise - Gallery defaults",
        default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
        default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
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
    shiny::outputOptions(output, "state_gallery_controls", suspendWhenHidden = FALSE)
    
    output$pct_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$n_pct > 0)
      def <- defaults[["pct"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "pct", n = rv$n_pct,
        title = "Percentage Change - Gallery defaults",
        default_width_in = def$width_in, default_height_in = def$height_in, default_dpi = def$dpi,
        default_transparent_bg = def$transparent_bg, default_panel_gap_px = def$panel_gap_px %||% def$gap_px %||% 16,
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
    shiny::outputOptions(output, "pct_gallery_controls", suspendWhenHidden = FALSE)
    
    # -------------------------------------------------------------------------
    # Plot galleries (per-plot cards)
    # -------------------------------------------------------------------------
    output$region_plots_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      shiny::req(rv$plots_generated, rv$plots)
      
      if (!rv$region_available) {
        return(div(
          class = "info-panel", style = "margin-top: 20px; padding: 30px; text-align: center;",
          h4("No region data available"),
          p("Please upload a regional mapping file or ensure your data contains a 'Region' column.")
        ))
      }
      
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
      
      if (!rv$region_available) {
        return(div(
          class = "info-panel", style = "margin-top: 20px; padding: 30px; text-align: center;",
          h4("No region data available"),
          p("Percentage change plots require a 'Region' column. Please add it via the optional mapping or in your main data.")
        ))
      }
      
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
    # Processed table
    # -------------------------------------------------------------------------
    output$processed_table <- DT::renderDT({
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      df <- st$obj$final_comp
      
      colnames_map <- c(
        LOCNUM      = "Locnum",
        LOCNAME     = "Locname",
        STATECODE   = "Statecode",
        COUNTY      = "County",
        key         = "Key",
        Classification = "Classification",
        Description = "Description",
        model_title = "Model Title",
        UniqueID    = "UniqueID",
        Region      = "Region",
        model_1     = "Model New",
        model_2     = "Model Old",
        model_new   = "Model New AAL",
        model_old   = "Model Old AAL"
      )
      # Reorder columns first (dropping any not in the map or not present in df),
      # then rename. This keeps Model New/Model Old/Model New AAL/Model Old AAL
      # pinned at the end regardless of their order in final_comp.
      ord <- intersect(names(colnames_map), names(df))
      df <- df[, ord, drop = FALSE]
      names(df) <- colnames_map[ord]
      
      # filter = "top" removed: DT's column-filter row loads its own bundled
      # selectize.js, which clobbers Shiny's global selectize plugin
      # registry (dropping "selectize-plugin-a11y") for the rest of the
      # browser session. Since this table lives on the Vulsen page itself,
      # leaving filter = "top" here could break OTHER Vulsen dropdowns
      # (subperil_ui, model_columns_ui) rendered afterward in the same
      # session -- no Result Extraction visit required to trigger it.
      # Default DT dom already includes a global search box.
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # -------------------------------------------------------------------------
    # Processed metadata
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
          div(style = "font-weight: 600;", "States:", length(unique(df$STATECODE))),
          if ("Region" %in% names(df)) {
            div(style = "font-weight: 600;", "Regions:", length(unique(df$Region[!is.na(df$Region)])))
          }
        )
      )
    })
    shiny::outputOptions(output, "processed_metadata", suspendWhenHidden = FALSE)
    
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
    
    # NEW: Download sample region mapping
    output$download_sample_region <- shiny::downloadHandler(
      filename = function() basename(sample_region_path),
      content = function(file) {
        if (!file.exists(sample_region_path)) stop("Sample region mapping file not found: ", sample_region_path)
        file.copy(sample_region_path, file, overwrite = TRUE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Helper function for strict HTML file naming
    # -------------------------------------------------------------------------
    build_html_filename <- function(ext = "html") {
      vendor <- switch(input$model_family,
                       "moody" = "Moodys",
                       "verisk" = "Verisk",
                       input$model_family)
      country <- input$country_code %||% "US"
      suffix <- input$suffix %||% "2026"
      peril <- input$peril %||% "Unknown"
      subperil <- input$subperil %||% "AllPeril"
      model1 <- input$model1_col %||% "Model1"
      model2 <- if (identical(input$n_models, "2")) input$model2_col else NULL
      date <- format(Sys.Date(), "%Y-%m-%d")
      
      sanitize <- function(x) {
        x <- gsub("[^A-Za-z0-9_]", "_", x)
        x <- gsub("_+", "_", x)
        x <- gsub("^_|_$", "", x)
        x
      }
      
      parts <- c(sanitize(vendor), sanitize(country), sanitize(suffix),
                 sanitize(peril), sanitize(subperil), sanitize(model1))
      if (!is.null(model2) && nzchar(model2)) {
        parts <- c(parts, sanitize(model2))
      }
      parts <- c(parts, date)
      filename <- paste(parts, collapse = "_")
      paste0(filename, ".", ext)
    }
    
    # -------------------------------------------------------------------------
    # Main download handler with prerequisites check and strict naming
    # -------------------------------------------------------------------------
    output$download_selected <- shiny::downloadHandler(
      filename = function() {
        choice <- input$download_type %||% "data"
        ext <- switch(choice,
                      "html_customized" = ".html",
                      "plots" = ".zip",
                      "data" = ".csv",
                      "all" = ".zip",
                      ".zip")
        if (choice %in% c("html_customized")) {
          return(build_html_filename("html"))
        } else {
          return(paste0("vulsen_output_", Sys.Date(), ext))
        }
      },
      content = function(file) {
        # ---- Prerequisites check ----
        if (!rv$data_loaded) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Download not ready",
              "Please load data first (upload a file or use the sample data).",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        if (!rv$plots_generated) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Download not ready",
              "Please generate plots by clicking 'Create Plot' first.",
              footer = shiny::modalButton("OK"),
              easyClose = TRUE
            )
          )
          return(NULL)
        }
        
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
        
        # ---- build_customized_html_bundles ----
        build_customized_html_bundles <- function() {
          out <- list(region = list(), state = list(), pct = list())
          for (group in c("region", "state", "pct")) {
            plot_list <- plots[[group]]
            if (is.null(plot_list) || !length(plot_list)) next
            def <- defaults[[group]]()
            for (key in names(plot_list)) {
              safe_key  <- sec_safe_id_key(key)
              base_plot <- plot_list[[key]]
              p <- vul_apply_overrides(base_plot, input, group, safe_key, def,
                                       class_order = class_order_committed[[paste0(group, "_", safe_key)]])
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
          template_path <- file.path("www", "vulnerability_customized_template.Rmd")
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
              n_regions    = if ("Region" %in% names(obj$final_comp)) length(unique(obj$final_comp$Region[!is.na(obj$final_comp$Region)])) else 0
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
            p <- vul_apply_overrides(base_plot, input, group, safe_key, def,
                                     class_order = class_order_committed[[paste0(group, "_", safe_key)]])
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
          
          html_file <- file.path(tmp_dir, build_html_filename("html"))
          save_vulsen_fallback_html(obj, plots, html_file)
          files_to_zip <- c(files_to_zip, html_file)
          
          html_customized_file <- file.path(tmp_dir, build_html_filename("html"))
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
              htmltools::tags$div(class = "metric-card", htmltools::tags$div(class = "metric-value", if ("Region" %in% names(obj$final_comp)) length(unique(obj$final_comp$Region[!is.na(obj$final_comp$Region)])) else 0), htmltools::tags$div(class = "metric-label", "Regions"))
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




