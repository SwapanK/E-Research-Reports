# =============================================================================
# server/Vulsen_server.R
# VulSen server - full module with Legend Configuration Manager, gallery
# overrides, per-card sizing, and downloads
# =============================================================================

Vulsen_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

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
      plots_generated = FALSE
    )

    # Default overrides per group (region, state, pct)
    defaults <- list(
      region = shiny::reactiveVal(vul_default_overrides("region")),
      state  = shiny::reactiveVal(vul_default_overrides("state")),
      pct    = shiny::reactiveVal(vul_default_overrides("pct"))
    )

    # -------------------------------------------------------------------------
    # Legend Configuration Manager - two independent reactive schemes.
    # rel_scheme is shared by every Regionwise AND Statewise heatmap; the
    # Statewise tab only ever shows a read-only preview of it. pct_scheme is
    # used only by the Percentage Change heatmaps.
    # -------------------------------------------------------------------------
    rel_scheme <- shiny::reactiveVal(REL_AAL_DEFAULT_BINS)
    pct_scheme <- shiny::reactiveVal(PCT_CHANGE_DEFAULT_BINS)

    legend_msg <- list(
      rel = shiny::reactiveVal(NULL),
      pct = shiny::reactiveVal(NULL)
    )

    # ---- Helper: wire up one legend tab's full set of observers ----
    setup_legend_manager <- function(tag, scheme_val, default_bins, scheme_type, value_source) {
      # tag = "rel" or "pct" ; used as the input-id prefix for the legend UI

      # -- Editable table render --
      output[[paste0(tag, "_legend_table")]] <- DT::renderDT({
        vul_legend_datatable(scheme_val())
      }, server = FALSE)

      # -- Cell edits update the reactive scheme --
      shiny::observeEvent(input[[paste0(tag, "_legend_table_cell_edit")]], {
        info <- input[[paste0(tag, "_legend_table_cell_edit")]]
        df <- scheme_val()
        df <- df[order(-suppressWarnings(as.numeric(df$level))), c("level", "label", "lower", "upper", "colour")]
        col_names <- c("level", "label", "lower", "upper", "colour")
        col <- col_names[info$col + 1]
        row <- info$row
        val <- info$value
        if (col %in% c("lower", "upper")) val <- suppressWarnings(as.numeric(val))
        df[row, col] <- val
        v <- vul_validate_scheme(df)
        if (isTRUE(v$ok)) {
          scheme_val(v$scheme)
          legend_msg[[tag]](NULL)
        } else {
          legend_msg[[tag]](v$msg)
        }
      })

      # -- Load Default --
      shiny::observeEvent(input[[paste0(tag, "_legend_load_default")]], {
        scheme_val(default_bins)
        legend_msg[[tag]](NULL)
        session$sendCustomMessage("show-toast", list(text = "Default legend loaded", type = "success", icon = "fa-rotate-left", duration = 1800))
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
        scheme_val(res$scheme)
        legend_msg[[tag]](NULL)
        session$sendCustomMessage("show-toast", list(text = "Legend JSON loaded", type = "success", icon = "fa-file-import", duration = 1800))
      })

      # -- Download JSON --
      output[[paste0(tag, "_legend_download_json")]] <- shiny::downloadHandler(
        filename = function() paste0("vulsen_", tag, "_legend_", Sys.Date(), ".json"),
        content = function(file) {
          json <- vul_scheme_to_json(scheme_val(), paste0(tag, "_scheme"), scheme_type)
          writeLines(as.character(json), file)
        }
      )

      # -- Create Bins (quantile-based, Problem 3) --
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
        scheme_val(res)
        legend_msg[[tag]](NULL)
        session$sendCustomMessage("show-toast", list(text = paste("Created", nrow(res), "quantile bins"), type = "success", icon = "fa-wand-magic-sparkles", duration = 2000))
      })

      # -- Validation message --
      output[[paste0(tag, "_legend_msg")]] <- shiny::renderUI({
        msg <- legend_msg[[tag]]()
        if (is.null(msg)) return(NULL)
        status_box("error", msg)
      })

      # -- Live preview swatches --
      output[[paste0(tag, "_legend_preview")]] <- shiny::renderUI({
        vul_legend_preview_tags(scheme_val())
      })
    }

    setup_legend_manager(
      "rel", rel_scheme, REL_AAL_DEFAULT_BINS, "relative_aal",
      value_source = function() {
        st <- obj_state()
        if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
        unlist(lapply(st$obj$region_split, function(d) d$value), use.names = FALSE)
      }
    )
    setup_legend_manager(
      "pct", pct_scheme, PCT_CHANGE_DEFAULT_BINS, "percentage_change",
      value_source = function() {
        shiny::req(rv$plots_generated)
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

    # Progress stepper
    output$progress_panel <- shiny::renderUI({
      stages <- list(
        list(label = "Model",    done = rv$model_loaded),
        list(label = "Data",     done = rv$data_loaded),
        list(label = "Plots",    done = rv$plots_generated),
        list(label = "Download", done = rv$plots_generated)
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
    # Model column name fields (depend on vendor + number of model versions)
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
    # Show/hide the Percentage Change tab based on number of model versions
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
    # Build analysis object
    # -------------------------------------------------------------------------
    obj_state <- shiny::reactive({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        return(list(ok = FALSE, msg = rs$msg, obj = NULL))
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
    # Status / Metrics UI
    # -------------------------------------------------------------------------
    output$status_ui <- shiny::renderUI({
      st <- obj_state()
      if (isTRUE(st$ok)) status_box("success", st$msg) else status_box("error", st$msg)
    })

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
    # Static ggplots
    # Built when the user clicks "Create Plot" in section 3, and rebuilt
    # automatically whenever the active Legend Configuration (Relative AAL
    # or Percentage Change scheme) changes afterwards - this is what powers
    # the Legend Configuration Manager's live preview across every plot.
    # -------------------------------------------------------------------------
    gen_trigger <- shiny::reactiveVal(0)
    shiny::observeEvent(input$create_plot, {
      gen_trigger(gen_trigger() + 1)
    })

    # Region + State only depend on rel_scheme() - Percentage Change only
    # depends on pct_scheme(). Kept as two separate reactives (rather than
    # one call to build_all_static_gplots()) so that, e.g., clicking
    # "Load Default" on the Percentage Change legend does NOT also
    # recompute every Regionwise/Statewise heatmap (and vice versa). Shiny
    # caches each reactive independently, so only the group whose scheme
    # actually changed gets rebuilt.
    static_plots_rel <- shiny::reactive({
      shiny::req(gen_trigger() > 0)
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      build_rel_state_gplots(st$obj, side_by_side = FALSE, rel_scheme = rel_scheme())
    })

    static_plots_pct <- shiny::reactive({
      shiny::req(gen_trigger() > 0)
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      build_pct_gplots(st$obj, pct_scheme = pct_scheme())
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
          bundles[[nm]] <- list(plot = gp[[nm]], width = def$export_w, height = def$export_h, dpi = def$dpi)
        }
        if (is.null(rv$bundles)) rv$bundles <- list()
        rv$bundles[[group]] <- bundles
      }

      rv$plots_generated <- TRUE

      # -----------------------------------------------------------------------
      # Render plot outputs for each group
      # -----------------------------------------------------------------------
      for (group in c("region", "state", "pct")) {
        local({
          grp <- group
          plot_list <- rv$plots[[grp]]
          if (is.null(plot_list) || length(plot_list) == 0) return()

          for (nm in names(plot_list)) {
            local({
              key <- nm
              prefix <- grp

              # ---- Plot frame: sizing is a container/CSS concern (Problem 9/10),
              # completely separate from export width/height used by ggsave.
              output[[paste0(prefix, "_plot_frame_", key)]] <- shiny::renderUI({
                def <- defaults[[prefix]]()
                disp_w  <- input[[paste0(prefix, "_display_w_pct_", key)]] %||% def$display_w_pct %||% 100
                disp_h  <- input[[paste0(prefix, "_display_h_px_", key)]] %||% def$display_h_px %||% 700
                max_w   <- input[[paste0(prefix, "_card_max_width_px_", key)]] %||% def$card_max_width_px %||% 900
                div(
                  style = sprintf("width:%s%%; max-width:%spx; margin:0 auto;", disp_w, max_w),
                  shiny::plotOutput(ns(paste0(prefix, "_plot_", key)), height = paste0(round(disp_h), "px"))
                )
              })

              output[[paste0(prefix, "_plot_", key)]] <- shiny::renderPlot({
                base_plot <- rv$plots[[prefix]][[key]]
                shiny::req(base_plot)
                def <- defaults[[prefix]]()
                p <- vul_apply_overrides(base_plot, input, prefix, key, def)
                p
              }, res = 96, bg = "white")

              output[[paste0(prefix, "_dl_", key)]] <- shiny::downloadHandler(
                filename = function() paste0("vulsen_", prefix, "_", key, "_", Sys.Date(), ".png"),
                content = function(file) {
                  base_plot <- rv$plots[[prefix]][[key]]
                  shiny::req(base_plot)
                  def <- defaults[[prefix]]()
                  p <- vul_apply_overrides(base_plot, input, prefix, key, def)
                  w <- input[[paste0(prefix, "_export_w_", key)]] %||% def$export_w %||% 9
                  h <- input[[paste0(prefix, "_export_h_", key)]] %||% def$export_h %||% 5
                  dpi <- input[[paste0(prefix, "_dpi_", key)]] %||% def$dpi %||% 150
                  bg <- attr(p, "vulsen_bg") %||% "white"
                  ggplot2::ggsave(file, plot = p, width = w, height = h, dpi = dpi, bg = bg, limitsize = FALSE)
                }
              )
            })
          }
        })
      }

      # -----------------------------------------------------------------------
      # "Apply to all" observers for each group's Gallery Defaults
      # -----------------------------------------------------------------------
      for (group in c("region", "state", "pct")) {
        local({
          grp <- group
          shiny::observeEvent(input[[paste0(grp, "_apply_all")]], {
            def <- list(
              export_w = input[[paste0(grp, "_default_export_w")]] %||% 9,
              export_h = input[[paste0(grp, "_default_export_h")]] %||% 5,
              dpi = input[[paste0(grp, "_default_dpi")]] %||% 150,
              display_w_pct = input[[paste0(grp, "_default_display_w_pct")]] %||% 100,
              display_h_px = input[[paste0(grp, "_default_display_h_px")]] %||% 700,
              card_max_width_px = input[[paste0(grp, "_default_card_max_width_px")]] %||% 900,
              axis_text = input[[paste0(grp, "_default_axis_text")]] %||% 12,
              axis_title = input[[paste0(grp, "_default_axis_title")]] %||% 14,
              plot_title = input[[paste0(grp, "_default_plot_title")]] %||% 16,
              strip_text = input[[paste0(grp, "_default_strip_text")]] %||% 12,
              strip_face = input[[paste0(grp, "_default_strip_face")]] %||% "bold",
              legend_text = input[[paste0(grp, "_default_legend_text")]] %||% 10,
              legend_title = input[[paste0(grp, "_default_legend_title")]] %||% 10,
              legend_pos = input[[paste0(grp, "_default_legend_pos")]] %||% "top",
              legend_show = input[[paste0(grp, "_legend_show")]] %||% TRUE,
              legend_key_size = input[[paste0(grp, "_default_legend_key_size")]] %||% 0.8,
              show_labels = input[[paste0(grp, "_default_show_labels")]] %||% TRUE,
              data_label_size = input[[paste0(grp, "_default_data_label_size")]] %||% 3.5,
              data_label_colour = input[[paste0(grp, "_default_data_label_colour")]] %||% "white",
              data_label_face = input[[paste0(grp, "_default_data_label_face")]] %||% "bold"
            )

            defaults[[grp]](def)

            plot_names <- names(rv$plots[[grp]])
            if (is.null(plot_names) || length(plot_names) == 0) return()

            for (key in plot_names) {
              shiny::updateNumericInput(session, paste0(grp, "_export_w_", key), value = def$export_w)
              shiny::updateNumericInput(session, paste0(grp, "_export_h_", key), value = def$export_h)
              shiny::updateNumericInput(session, paste0(grp, "_dpi_", key), value = def$dpi)
              shiny::updateNumericInput(session, paste0(grp, "_display_w_pct_", key), value = def$display_w_pct)
              shiny::updateNumericInput(session, paste0(grp, "_display_h_px_", key), value = def$display_h_px)
              shiny::updateNumericInput(session, paste0(grp, "_card_max_width_px_", key), value = def$card_max_width_px)
              shiny::updateNumericInput(session, paste0(grp, "_axis_text_", key), value = def$axis_text)
              shiny::updateNumericInput(session, paste0(grp, "_axis_title_", key), value = def$axis_title)
              shiny::updateNumericInput(session, paste0(grp, "_plot_title_", key), value = def$plot_title)
              if (grp != "pct") {
                shiny::updateNumericInput(session, paste0(grp, "_strip_text_", key), value = def$strip_text)
                shiny::updateSelectInput(session, paste0(grp, "_strip_face_", key), selected = def$strip_face)
              }
              shiny::updateNumericInput(session, paste0(grp, "_legend_text_", key), value = def$legend_text)
              shiny::updateNumericInput(session, paste0(grp, "_legend_title_", key), value = def$legend_title)
              shiny::updateSelectInput(session, paste0(grp, "_legend_pos_", key), selected = def$legend_pos)
              shiny::updateCheckboxInput(session, paste0(grp, "_legend_show_", key), value = def$legend_show)
              shiny::updateNumericInput(session, paste0(grp, "_legend_key_size_", key), value = def$legend_key_size)
              shiny::updateCheckboxInput(session, paste0(grp, "_show_labels_", key), value = def$show_labels)
              shiny::updateNumericInput(session, paste0(grp, "_data_label_size_", key), value = def$data_label_size)
              colourpicker::updateColourInput(session, paste0(grp, "_data_label_colour_", key), value = def$data_label_colour)
              shiny::updateSelectInput(session, paste0(grp, "_data_label_face_", key), selected = def$data_label_face)
            }

            session$sendCustomMessage("show-toast", list(text = paste("Applied defaults to all", grp, "plots"), type = "success", icon = "fa-wand-magic-sparkles", duration = 2000))
          })
        })
      }

    }) # end of observe for static_plots

    # -------------------------------------------------------------------------
    # GALLERY-LEVEL "apply to all" CONTROLLERS + Legend Configuration UI
    # (one per group). The Regionwise tab owns the editable Relative AAL
    # legend editor; the Statewise tab shares the same scheme read-only, so
    # there is exactly one place to edit it (Problem 7). The Percentage
    # Change tab owns its own, fully independent, editable legend.
    # -------------------------------------------------------------------------
    output$region_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["region"]])
      def <- defaults[["region"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "region", n = length(rv$plots[["region"]]),
        title = "Regionwise - Gallery defaults",
        default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
        default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
        default_card_max_width_px = def$card_max_width_px,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text, default_strip_face = def$strip_face,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_pos = def$legend_pos, default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
        has_facets = TRUE,
        legend_ui = vul_legend_config_ui(session$ns, "rel", "Legend Configuration - Relative AAL", editable = TRUE)
      )
    })

    output$state_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["state"]])
      def <- defaults[["state"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "state", n = length(rv$plots[["state"]]),
        title = "Statewise - Gallery defaults",
        default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
        default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
        default_card_max_width_px = def$card_max_width_px,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text, default_strip_face = def$strip_face,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_pos = def$legend_pos, default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
        has_facets = TRUE,
        legend_ui = vul_legend_config_ui(
          session$ns, "rel", "Legend Configuration - Relative AAL", editable = FALSE,
          readonly_note = "This legend is shared with the Regionwise tab. Edit it there - changes apply to both."
        )
      )
    })

    output$pct_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["pct"]])
      def <- defaults[["pct"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "pct", n = length(rv$plots[["pct"]]),
        title = "Percentage Change - Gallery defaults",
        default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
        default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
        default_card_max_width_px = def$card_max_width_px,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_legend_pos = def$legend_pos, default_legend_key_size = def$legend_key_size,
        default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
        default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
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
            default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
            default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
            default_card_max_width_px = def$card_max_width_px,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text, default_strip_face = def$strip_face,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
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
            default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
            default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
            default_card_max_width_px = def$card_max_width_px,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text, default_strip_face = def$strip_face,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
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
            default_export_w = def$export_w, default_export_h = def$export_h, default_dpi = def$dpi,
            default_display_w_pct = def$display_w_pct, default_display_h_px = def$display_h_px,
            default_card_max_width_px = def$card_max_width_px,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_legend_key_size = def$legend_key_size,
            default_show_labels = def$show_labels, default_data_label_size = def$data_label_size,
            default_data_label_colour = def$data_label_colour, default_data_label_face = def$data_label_face,
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
      DT::datatable(st$obj$final_comp, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE, filter = "top")
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
        ext <- switch(choice, "html" = ".html", "plots" = ".zip", "data" = ".csv", "all" = ".zip", ".zip")
        paste0("vulsen_output_", Sys.Date(), ext)
      },
      content = function(file) {
        st <- obj_state()
        shiny::req(isTRUE(st$ok), st$obj)
        obj <- st$obj
        plots <- rv$plots
        choice <- input$download_type %||% "data"

        if (choice == "data") {
          utils::write.csv(obj$final_comp, file, row.names = FALSE)
          return(invisible(file))
        }

        if (choice == "html") {
          can_use_template <- exists("save_html_report", mode = "function") && file.exists(file.path("templates", "vulsens_report.Rmd"))
          if (can_use_template) save_html_report(obj, file) else save_vulsen_fallback_html(obj, plots, file)
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
            base_plot <- plot_list[[key]]
            def <- defaults[[group]]()
            p <- vul_apply_overrides(base_plot, input, group, key, def)
            w <- input[[paste0(group, "_export_w_", key)]] %||% def$export_w %||% 9
            h <- input[[paste0(group, "_export_h_", key)]] %||% def$export_h %||% 5
            dpi <- input[[paste0(group, "_dpi_", key)]] %||% def$dpi %||% 150
            bg <- attr(p, "vulsen_bg") %||% "white"
            fname <- paste0(group, "_", key, ".png")
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

          rel_json <- file.path(tmp_dir, "relative_aal_legend.json")
          writeLines(as.character(vul_scheme_to_json(rel_scheme(), "relative_aal_scheme", "relative_aal")), rel_json)
          files_to_zip <- c(files_to_zip, rel_json)

          pct_json <- file.path(tmp_dir, "percentage_change_legend.json")
          writeLines(as.character(vul_scheme_to_json(pct_scheme(), "percentage_change_scheme", "percentage_change")), pct_json)
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
          ggplot2::ggsave(img_file, plot = plot_list[[nm]], width = 14, height = 8, dpi = 180, limitsize = FALSE)
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
