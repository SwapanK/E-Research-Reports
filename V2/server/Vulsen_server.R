# =============================================================================
# server/Vulsen_server.R
# VulSen server – full module with overrides, gallery, and downloads
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
      region = shiny::reactiveVal(vul_default_overrides()),
      state  = shiny::reactiveVal(vul_default_overrides()),
      pct    = shiny::reactiveVal(vul_default_overrides())
    )
    
    # -------------------------------------------------------------------------
    # Sample data paths
    # -------------------------------------------------------------------------
    sample_moody_path <- file.path(
      "data",
      "VulsenAPP_sample",
      "USSCS_RMS_HDv1_DLM_AAL_AllPeril_vulsen_comp_CCMasonary_SAMPLE.csv"
    )
    sample_verisk_path <- file.path(
      "data",
      "VulsenAPP_sample",
      "USSCS_Verisk_v12v13_vulsen_comp_AllPeril_SAMPLE.csv"
    )
    
    # -------------------------------------------------------------------------
    # Status helpers
    # -------------------------------------------------------------------------
    output$model_status <- shiny::renderUI({
      if (rv$model_loaded) {
        tags$span(class = "sec2-status done", icon("check"), "Loaded")
      } else {
        tags$span(class = "sec2-status pending", icon("clock"), "Pending")
      }
    })
    
    output$data_status <- shiny::renderUI({
      if (rv$data_loaded) {
        tags$span(class = "sec2-status done", icon("check"), "Ready")
      } else {
        tags$span(class = "sec2-status pending", icon("clock"), "Pending")
      }
    })
    
    output$download_status <- shiny::renderUI({
      if (rv$plots_generated) {
        tags$span(class = "sec2-status done", icon("check"), "Plots ready")
      } else {
        tags$span(class = "sec2-status pending", icon("clock"), "Waiting")
      }
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
            circle <- if (is_done) {
              tags$span(class = "sec-step-circle done", icon("check"))
            } else {
              tags$span(class = "sec-step-circle pending", i)
            }
            label <- tags$div(class = "sec-step-label", st$label)
            connector <- if (i < length(stages)) {
              tags$div(
                class = paste("sec-connector", if (is_done && stages[[i+1]]$done) "done" else "pending")
              )
            } else NULL
            tags$div(
              class = "sec-step-clickable",
              style = "display: flex; align-items: center; flex: 1;",
              onclick = sprintf("scrollToSection('%s')", ns(paste0("step_", tolower(st$label)))),
              tags$div(
                style = "display: flex; flex-direction: column; align-items: center;",
                circle,
                label
              ),
              connector
            )
          })
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Version text
    # -------------------------------------------------------------------------
    output$version_text <- shiny::renderUI({
      if (identical(input$model_family, "moody")) {
        shiny::tags$strong("Versions: HDv1 vs RLv25")
      } else {
        shiny::tags$strong("Versions: v13 vs v12")
      }
    })
    
    # -------------------------------------------------------------------------
    # Data load state
    # -------------------------------------------------------------------------
    sample_loaded <- shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(input$load_sample, {
      sample_loaded(TRUE)
    })
    
    raw_state <- shiny::reactive({
      if (!is.null(input$file)) {
        return(
          tryCatch(
            {
              df <- read_vulsens_file(input$file$datapath)
              list(
                ok   = TRUE,
                msg  = paste("Loaded uploaded file:", input$file$name),
                data = df
              )
            },
            error = function(e) {
              list(
                ok   = FALSE,
                msg  = paste("Could not read file:", conditionMessage(e)),
                data = NULL
              )
            }
          )
        )
      }
      
      if (isTRUE(sample_loaded())) {
        path <- if (identical(input$model_family, "moody")) {
          sample_moody_path
        } else {
          sample_verisk_path
        }
        return(
          tryCatch(
            {
              df <- read_vulsens_file(path)
              list(
                ok = TRUE,
                msg = paste(
                  "Loaded sample data for",
                  ifelse(identical(input$model_family, "moody"), "Moody's / RMS", "Verisk")
                ),
                data = df
              )
            },
            error = function(e) {
              list(
                ok   = FALSE,
                msg  = paste("Could not load sample data:", conditionMessage(e)),
                data = NULL
              )
            }
          )
        )
      }
      
      list(
        ok   = FALSE,
        msg  = "Upload a file or click 'Load Included Sample Data' to begin.",
        data = NULL
      )
    })
    
    # -------------------------------------------------------------------------
    # Build analysis object
    # -------------------------------------------------------------------------
    obj_state <- shiny::reactive({
      rs <- raw_state()
      if (!isTRUE(rs$ok) || is.null(rs$data)) {
        return(list(ok = FALSE, msg = rs$msg, obj = NULL))
      }
      tryCatch(
        {
          o <- build_rmd_objects(rs$data, input$model_family)
          rv$model_loaded <- TRUE
          rv$data_loaded <- TRUE
          list(
            ok  = TRUE,
            msg = rs$msg,
            obj = o
          )
        },
        error = function(e) {
          rv$model_loaded <- FALSE
          rv$data_loaded <- FALSE
          list(
            ok = FALSE,
            msg = paste(
              "Validation failed:",
              conditionMessage(e),
              "Please check model selection and column names in the uploaded file."
            ),
            obj = NULL
          )
        }
      )
    })
    
    # -------------------------------------------------------------------------
    # Status UI
    # -------------------------------------------------------------------------
    output$status_ui <- shiny::renderUI({
      st <- obj_state()
      if (isTRUE(st$ok)) {
        status_box("success", st$msg)
      } else {
        status_box("error", st$msg)
      }
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
        shiny::column(3, metric_card(length(unique(o$final_comp$Region)), "Regions")),
        shiny::column(3, metric_card(o$model_title, "Model family"))
      )
    })
    
    # -------------------------------------------------------------------------
    # Static ggplots
    # -------------------------------------------------------------------------
    static_plots <- shiny::reactive({
      st <- obj_state()
      shiny::req(isTRUE(st$ok), st$obj)
      build_all_static_gplots(st$obj, side_by_side = isTRUE(input$side_by_side))
    })
    
    # -------------------------------------------------------------------------
    # Build bundles and set up plot rendering
    # -------------------------------------------------------------------------
    shiny::observe({
      plots <- static_plots()
      shiny::req(plots)
      
      rv$plots <- plots
      
      for (group in c("region", "state", "pct")) {
        if (group == "pct") {
          gp <- plots[["pct"]]
        } else {
          gp <- plots[[group]]
        }
        if (is.null(gp) || length(gp) == 0) next
        def <- defaults[[group]]()
        bundles <- list()
        for (nm in names(gp)) {
          bundles[[nm]] <- list(
            plot = gp[[nm]],
            width = def$w %||% 9,
            height = def$h %||% 5,
            dpi = def$dpi %||% 150,
            bg = def$bg %||% "white"
          )
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
          plot_list <- if (grp == "pct") {
            rv$plots[["pct"]]
          } else {
            rv$plots[[grp]]
          }
          if (is.null(plot_list) || length(plot_list) == 0) return()
          
          for (nm in names(plot_list)) {
            local({
              key <- nm
              prefix <- grp
              
              output[[paste0(prefix, "_plot_frame_", key)]] <- shiny::renderUI({
                h_in <- (input[[paste0(prefix, "_h_", key)]] %||% defaults[[prefix]]()$h %||% 5)
                plotly::plotlyOutput(ns(paste0(prefix, "_plot_", key)), height = paste0(round(h_in * 96), "px"))
              })
              
              output[[paste0(prefix, "_plot_", key)]] <- plotly::renderPlotly({
                base_plot <- if (prefix == "pct") {
                  rv$plots[["pct"]][[key]]
                } else {
                  rv$plots[[prefix]][[key]]
                }
                shiny::req(base_plot)
                
                def <- defaults[[prefix]]()
                override_vals <- list(
                  axis_text = input[[paste0(prefix, "_axis_text_", key)]] %||% def$axis_text %||% 12,
                  axis_title = input[[paste0(prefix, "_axis_title_", key)]] %||% def$axis_title %||% 14,
                  plot_title = input[[paste0(prefix, "_plot_title_", key)]] %||% def$plot_title %||% 16,
                  strip_text = input[[paste0(prefix, "_strip_text_", key)]] %||% def$strip_text %||% 12,
                  legend_text = input[[paste0(prefix, "_legend_text_", key)]] %||% def$legend_text %||% 10,
                  legend_title = input[[paste0(prefix, "_legend_title_", key)]] %||% def$legend_title %||% 10,
                  axis_angle = input[[paste0(prefix, "_axis_angle_", key)]] %||% def$axis_angle %||% 90,
                  legend_pos = input[[paste0(prefix, "_legend_pos_", key)]] %||% def$legend_pos %||% "top",
                  legend_show = input[[paste0(prefix, "_legend_show_", key)]] %||% def$legend_show %||% TRUE,
                  legend_key_size = input[[paste0(prefix, "_legend_key_size_", key)]] %||% def$legend_key_size %||% 0.8,
                  title_hjust = input[[paste0(prefix, "_plot_title_hjust_", key)]] %||% def$title_hjust %||% 0.5,
                  axis_line_col = input[[paste0(prefix, "_axis_line_col_", key)]] %||% def$axis_line_col %||% "black",
                  panel_fill = input[[paste0(prefix, "_panel_fill_", key)]] %||% def$panel_fill %||% "white",
                  bg = input[[paste0(prefix, "_bg_", key)]] %||% def$bg %||% "white",
                  grid_col = input[[paste0(prefix, "_grid_col_", key)]] %||% def$grid_col %||% "#e9ecf3",
                  panel_spacing = input[[paste0(prefix, "_panel_spacing_", key)]] %||% def$panel_spacing %||% 3,
                  margin_t = input[[paste0(prefix, "_plot_margin_t_", key)]] %||% def$margin_t %||% 30,
                  margin_r = input[[paste0(prefix, "_plot_margin_r_", key)]] %||% def$margin_r %||% 10,
                  margin_b = input[[paste0(prefix, "_plot_margin_b_", key)]] %||% def$margin_b %||% 30,
                  margin_l = input[[paste0(prefix, "_plot_margin_l_", key)]] %||% def$margin_l %||% 10,
                  border_col = input[[paste0(prefix, "_panel_border_col_", key)]] %||% def$border_col %||% "black",
                  border_lwd = input[[paste0(prefix, "_panel_border_lwd_", key)]] %||% def$border_lwd %||% 0.5,
                  axis_text_margin_t = input[[paste0(prefix, "_axis_text_margin_t_", key)]] %||% def$axis_text_margin_t %||% 5,
                  axis_text_vjust = input[[paste0(prefix, "_axis_text_vjust_", key)]] %||% def$axis_text_vjust %||% 0.5,
                  col_sfd = input[[paste0(prefix, "_col_sfd_", key)]] %||% def$col_sfd %||% "#6FACDE",
                  col_com = input[[paste0(prefix, "_col_com_", key)]] %||% def$col_com %||% "#F0B323",
                  col_pen = input[[paste0(prefix, "_col_pen_", key)]] %||% def$col_pen %||% "#F0B323",
                  col_cred = input[[paste0(prefix, "_col_cred_", key)]] %||% def$col_cred %||% "#6FACDE"
                )
                
                p <- vul_apply_overrides(base_plot, input, prefix, key, override_vals)
                gg_to_plotly(p)
              })
              
              output[[paste0(prefix, "_dl_", key)]] <- shiny::downloadHandler(
                filename = function() {
                  paste0("vulsen_", prefix, "_", key, "_", Sys.Date(), ".png")
                },
                content = function(file) {
                  base_plot <- if (prefix == "pct") {
                    rv$plots[["pct"]][[key]]
                  } else {
                    rv$plots[[prefix]][[key]]
                  }
                  shiny::req(base_plot)
                  def <- defaults[[prefix]]()
                  override_vals <- list(
                    axis_text = input[[paste0(prefix, "_axis_text_", key)]] %||% def$axis_text %||% 12,
                    axis_title = input[[paste0(prefix, "_axis_title_", key)]] %||% def$axis_title %||% 14,
                    plot_title = input[[paste0(prefix, "_plot_title_", key)]] %||% def$plot_title %||% 16,
                    strip_text = input[[paste0(prefix, "_strip_text_", key)]] %||% def$strip_text %||% 12,
                    legend_text = input[[paste0(prefix, "_legend_text_", key)]] %||% def$legend_text %||% 10,
                    legend_title = input[[paste0(prefix, "_legend_title_", key)]] %||% def$legend_title %||% 10,
                    axis_angle = input[[paste0(prefix, "_axis_angle_", key)]] %||% def$axis_angle %||% 90,
                    legend_pos = input[[paste0(prefix, "_legend_pos_", key)]] %||% def$legend_pos %||% "top",
                    legend_show = input[[paste0(prefix, "_legend_show_", key)]] %||% def$legend_show %||% TRUE,
                    legend_key_size = input[[paste0(prefix, "_legend_key_size_", key)]] %||% def$legend_key_size %||% 0.8,
                    title_hjust = input[[paste0(prefix, "_plot_title_hjust_", key)]] %||% def$title_hjust %||% 0.5,
                    axis_line_col = input[[paste0(prefix, "_axis_line_col_", key)]] %||% def$axis_line_col %||% "black",
                    panel_fill = input[[paste0(prefix, "_panel_fill_", key)]] %||% def$panel_fill %||% "white",
                    bg = input[[paste0(prefix, "_bg_", key)]] %||% def$bg %||% "white",
                    grid_col = input[[paste0(prefix, "_grid_col_", key)]] %||% def$grid_col %||% "#e9ecf3",
                    panel_spacing = input[[paste0(prefix, "_panel_spacing_", key)]] %||% def$panel_spacing %||% 3,
                    margin_t = input[[paste0(prefix, "_plot_margin_t_", key)]] %||% def$margin_t %||% 30,
                    margin_r = input[[paste0(prefix, "_plot_margin_r_", key)]] %||% def$margin_r %||% 10,
                    margin_b = input[[paste0(prefix, "_plot_margin_b_", key)]] %||% def$margin_b %||% 30,
                    margin_l = input[[paste0(prefix, "_plot_margin_l_", key)]] %||% def$margin_l %||% 10,
                    border_col = input[[paste0(prefix, "_panel_border_col_", key)]] %||% def$border_col %||% "black",
                    border_lwd = input[[paste0(prefix, "_panel_border_lwd_", key)]] %||% def$border_lwd %||% 0.5,
                    axis_text_margin_t = input[[paste0(prefix, "_axis_text_margin_t_", key)]] %||% def$axis_text_margin_t %||% 5,
                    axis_text_vjust = input[[paste0(prefix, "_axis_text_vjust_", key)]] %||% def$axis_text_vjust %||% 0.5,
                    col_sfd = input[[paste0(prefix, "_col_sfd_", key)]] %||% def$col_sfd %||% "#6FACDE",
                    col_com = input[[paste0(prefix, "_col_com_", key)]] %||% def$col_com %||% "#F0B323",
                    col_pen = input[[paste0(prefix, "_col_pen_", key)]] %||% def$col_pen %||% "#F0B323",
                    col_cred = input[[paste0(prefix, "_col_cred_", key)]] %||% def$col_cred %||% "#6FACDE"
                  )
                  p <- vul_apply_overrides(base_plot, input, prefix, key, override_vals)
                  w <- (input[[paste0(prefix, "_w_", key)]] %||% def$w %||% 9)
                  h <- (input[[paste0(prefix, "_h_", key)]] %||% def$h %||% 5)
                  dpi <- (input[[paste0(prefix, "_dpi_", key)]] %||% def$dpi %||% 150)
                  bg <- attr(p, "vulsen_bg") %||% "white"
                  ggplot2::ggsave(file, plot = p, width = w, height = h, dpi = dpi, bg = bg, limitsize = FALSE)
                }
              )
            })
          }
        })
      }
      
      # -----------------------------------------------------------------------
      # "Apply to all" observers for each group
      # -----------------------------------------------------------------------
      for (group in c("region", "state", "pct")) {
        local({
          grp <- group
          shiny::observeEvent(input[[paste0(grp, "_apply_all")]], {
            def <- list(
              w = input[[paste0(grp, "_default_w")]] %||% 9,
              h = input[[paste0(grp, "_default_h")]] %||% 5,
              dpi = input[[paste0(grp, "_default_dpi")]] %||% 150,
              bg = input[[paste0(grp, "_default_bg")]] %||% "white",
              axis_text = input[[paste0(grp, "_default_axis_text")]] %||% 12,
              axis_title = input[[paste0(grp, "_default_axis_title")]] %||% 14,
              plot_title = input[[paste0(grp, "_default_plot_title")]] %||% 16,
              strip_text = input[[paste0(grp, "_default_strip_text")]] %||% 12,
              legend_text = input[[paste0(grp, "_default_legend_text")]] %||% 10,
              legend_title = input[[paste0(grp, "_default_legend_title")]] %||% 10,
              axis_angle = input[[paste0(grp, "_default_axis_angle")]] %||% 90,
              legend_pos = input[[paste0(grp, "_default_legend_pos")]] %||% "top",
              legend_show = input[[paste0(grp, "_legend_show")]] %||% TRUE,
              legend_key_size = input[[paste0(grp, "_default_legend_key_size")]] %||% 0.8,
              title_hjust = input[[paste0(grp, "_default_title_hjust")]] %||% 0.5,
              axis_line_col = input[[paste0(grp, "_default_axis_line_col")]] %||% "black",
              panel_fill = input[[paste0(grp, "_default_panel_fill")]] %||% "white",
              grid_col = input[[paste0(grp, "_default_grid_col")]] %||% "#e9ecf3",
              panel_spacing = input[[paste0(grp, "_default_panel_spacing")]] %||% 3,
              margin_t = input[[paste0(grp, "_default_margin_t")]] %||% 30,
              margin_r = input[[paste0(grp, "_default_margin_r")]] %||% 10,
              margin_b = input[[paste0(grp, "_default_margin_b")]] %||% 30,
              margin_l = input[[paste0(grp, "_default_margin_l")]] %||% 10,
              border_col = input[[paste0(grp, "_default_border_col")]] %||% "black",
              border_lwd = input[[paste0(grp, "_default_border_lwd")]] %||% 0.5,
              col_sfd = input[[paste0(grp, "_default_col_sfd")]] %||% "#6FACDE",
              col_com = input[[paste0(grp, "_default_col_com")]] %||% "#F0B323",
              col_pen = input[[paste0(grp, "_default_col_pen")]] %||% "#F0B323",
              col_cred = input[[paste0(grp, "_default_col_cred")]] %||% "#6FACDE",
              axis_text_margin_t = input[[paste0(grp, "_default_axis_text_margin_t")]] %||% 5,
              axis_text_vjust = input[[paste0(grp, "_default_axis_text_vjust")]] %||% 0.5
            )
            
            defaults[[grp]](def)
            
            plot_names <- if (grp == "pct") {
              names(rv$plots[["pct"]])
            } else {
              names(rv$plots[[grp]])
            }
            if (is.null(plot_names) || length(plot_names) == 0) return()
            
            for (key in plot_names) {
              shiny::updateNumericInput(session, paste0(grp, "_w_", key), value = def$w)
              shiny::updateNumericInput(session, paste0(grp, "_h_", key), value = def$h)
              shiny::updateNumericInput(session, paste0(grp, "_dpi_", key), value = def$dpi)
              shiny::updateSelectInput(session, paste0(grp, "_bg_", key), selected = def$bg)
              shiny::updateNumericInput(session, paste0(grp, "_axis_text_", key), value = def$axis_text)
              shiny::updateNumericInput(session, paste0(grp, "_axis_title_", key), value = def$axis_title)
              shiny::updateNumericInput(session, paste0(grp, "_plot_title_", key), value = def$plot_title)
              shiny::updateNumericInput(session, paste0(grp, "_strip_text_", key), value = def$strip_text)
              shiny::updateNumericInput(session, paste0(grp, "_legend_text_", key), value = def$legend_text)
              shiny::updateNumericInput(session, paste0(grp, "_legend_title_", key), value = def$legend_title)
              shiny::updateNumericInput(session, paste0(grp, "_axis_angle_", key), value = def$axis_angle)
              shiny::updateSelectInput(session, paste0(grp, "_legend_pos_", key), selected = def$legend_pos)
              shiny::updateCheckboxInput(session, paste0(grp, "_legend_show_", key), value = def$legend_show)
              shiny::updateNumericInput(session, paste0(grp, "_legend_key_size_", key), value = def$legend_key_size)
              shiny::updateNumericInput(session, paste0(grp, "_plot_title_hjust_", key), value = def$title_hjust)
              colourpicker::updateColourInput(session, paste0(grp, "_axis_line_col_", key), value = def$axis_line_col)
              colourpicker::updateColourInput(session, paste0(grp, "_panel_fill_", key), value = def$panel_fill)
              colourpicker::updateColourInput(session, paste0(grp, "_grid_col_", key), value = def$grid_col)
              shiny::updateNumericInput(session, paste0(grp, "_panel_spacing_", key), value = def$panel_spacing)
              shiny::updateNumericInput(session, paste0(grp, "_plot_margin_t_", key), value = def$margin_t)
              shiny::updateNumericInput(session, paste0(grp, "_plot_margin_r_", key), value = def$margin_r)
              shiny::updateNumericInput(session, paste0(grp, "_plot_margin_b_", key), value = def$margin_b)
              shiny::updateNumericInput(session, paste0(grp, "_plot_margin_l_", key), value = def$margin_l)
              colourpicker::updateColourInput(session, paste0(grp, "_panel_border_col_", key), value = def$border_col)
              shiny::updateNumericInput(session, paste0(grp, "_panel_border_lwd_", key), value = def$border_lwd)
              colourpicker::updateColourInput(session, paste0(grp, "_col_sfd_", key), value = def$col_sfd)
              colourpicker::updateColourInput(session, paste0(grp, "_col_com_", key), value = def$col_com)
              colourpicker::updateColourInput(session, paste0(grp, "_col_pen_", key), value = def$col_pen)
              colourpicker::updateColourInput(session, paste0(grp, "_col_cred_", key), value = def$col_cred)
              shiny::updateNumericInput(session, paste0(grp, "_axis_text_margin_t_", key), value = def$axis_text_margin_t)
              shiny::updateNumericInput(session, paste0(grp, "_axis_text_vjust_", key), value = def$axis_text_vjust)
            }
            
            session$sendCustomMessage("show-toast", list(
              text = paste("Applied defaults to all", grp, "plots"),
              type = "success",
              icon = "fa-wand-magic-sparkles",
              duration = 2000
            ))
          })
        })
      }
      
    }) # end of observe for static_plots
    
    # -------------------------------------------------------------------------
    # GALLERY-LEVEL "apply to all" CONTROLLERS (one per group)
    # -------------------------------------------------------------------------
    output$region_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["region"]])
      def <- defaults[["region"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "region", n = length(rv$plots[["region"]]),
        title = "Region – Gallery defaults",
        default_w = def$w, default_h = def$h, default_dpi = def$dpi, default_bg = def$bg,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_axis_angle = def$axis_angle, default_legend_pos = def$legend_pos,
        default_legend_key_size = def$legend_key_size, default_title_hjust = def$title_hjust,
        default_axis_line_col = def$axis_line_col, default_panel_fill = def$panel_fill,
        default_grid_col = def$grid_col, default_panel_spacing = def$panel_spacing,
        default_margin_t = def$margin_t, default_margin_r = def$margin_r,
        default_margin_b = def$margin_b, default_margin_l = def$margin_l,
        default_border_col = def$border_col, default_border_lwd = def$border_lwd,
        default_col_sfd = def$col_sfd, default_col_com = def$col_com,
        default_col_pen = def$col_pen, default_col_cred = def$col_cred,
        default_axis_text_margin_t = def$axis_text_margin_t, default_axis_text_vjust = def$axis_text_vjust
      )
    })
    
    output$state_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["state"]])
      def <- defaults[["state"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "state", n = length(rv$plots[["state"]]),
        title = "State – Gallery defaults",
        default_w = def$w, default_h = def$h, default_dpi = def$dpi, default_bg = def$bg,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_axis_angle = def$axis_angle, default_legend_pos = def$legend_pos,
        default_legend_key_size = def$legend_key_size, default_title_hjust = def$title_hjust,
        default_axis_line_col = def$axis_line_col, default_panel_fill = def$panel_fill,
        default_grid_col = def$grid_col, default_panel_spacing = def$panel_spacing,
        default_margin_t = def$margin_t, default_margin_r = def$margin_r,
        default_margin_b = def$margin_b, default_margin_l = def$margin_l,
        default_border_col = def$border_col, default_border_lwd = def$border_lwd,
        default_col_sfd = def$col_sfd, default_col_com = def$col_com,
        default_col_pen = def$col_pen, default_col_cred = def$col_cred,
        default_axis_text_margin_t = def$axis_text_margin_t, default_axis_text_vjust = def$axis_text_vjust
      )
    })
    
    output$pct_gallery_controls <- shiny::renderUI({
      shiny::req(rv$plots_generated, rv$plots[["pct"]])
      def <- defaults[["pct"]]()
      vul_gallery_controls_ui(
        ns = session$ns, prefix = "pct", n = length(rv$plots[["pct"]]),
        title = "Percentage Change – Gallery defaults",
        default_w = def$w, default_h = def$h, default_dpi = def$dpi, default_bg = def$bg,
        default_axis_text = def$axis_text, default_axis_title = def$axis_title,
        default_plot_title = def$plot_title, default_strip_text = def$strip_text,
        default_legend_text = def$legend_text, default_legend_title = def$legend_title,
        default_axis_angle = def$axis_angle, default_legend_pos = def$legend_pos,
        default_legend_key_size = def$legend_key_size, default_title_hjust = def$title_hjust,
        default_axis_line_col = def$axis_line_col, default_panel_fill = def$panel_fill,
        default_grid_col = def$grid_col, default_panel_spacing = def$panel_spacing,
        default_margin_t = def$margin_t, default_margin_r = def$margin_r,
        default_margin_b = def$margin_b, default_margin_l = def$margin_l,
        default_border_col = def$border_col, default_border_lwd = def$border_lwd,
        default_col_sfd = def$col_sfd, default_col_com = def$col_com,
        default_col_pen = def$col_pen, default_col_cred = def$col_cred,
        default_axis_text_margin_t = def$axis_text_margin_t, default_axis_text_vjust = def$axis_text_vjust
      )
    })
    
    # -------------------------------------------------------------------------
    # RENDER THE PLOT UI (with namespace fix)
    # -------------------------------------------------------------------------
    output$region_plots_ui <- shiny::renderUI({
      st <- obj_state()
      if (!isTRUE(st$ok) || is.null(st$obj)) return(NULL)
      shiny::req(rv$plots_generated, rv$plots)
      
      p <- rv$plots
      rnames <- intersect(PLOT_ORDER, names(p$region))
      if (length(rnames) == 0) {
        return(
          div(
            class = "info-panel",
            style = "margin-top: 20px; padding: 30px; text-align: center;",
            h4("No regionwise plots could be generated"),
            p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
            p("Check the R console for diagnostic messages showing the unique Classification values."),
            p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
          )
        )
      }
      
      shiny::tagList(
        shiny::h2("Regionwise Comparison"),
        lapply(rnames, function(nm) {
          def <- defaults[["region"]]()
          vul_plot_card_gallery(
            ns = session$ns,      # <-- NAMESPACE FIX
            key = nm,
            label = nm,
            prefix = "region",
            default_w = def$w, default_h = def$h,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_axis_angle = def$axis_angle, default_legend_key_size = def$legend_key_size,
            default_title_hjust = def$title_hjust, default_panel_spacing = def$panel_spacing,
            default_margin_t = def$margin_t, default_margin_r = def$margin_r,
            default_margin_b = def$margin_b, default_margin_l = def$margin_l,
            default_border_lwd = def$border_lwd, default_axis_line_col = def$axis_line_col,
            default_panel_fill = def$panel_fill, default_grid_col = def$grid_col,
            default_border_col = def$border_col, default_col_sfd = def$col_sfd,
            default_col_com = def$col_com, default_col_pen = def$col_pen,
            default_col_cred = def$col_cred, default_axis_text_margin_t = def$axis_text_margin_t,
            default_axis_text_vjust = def$axis_text_vjust
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
        return(
          div(
            class = "info-panel",
            style = "margin-top: 20px; padding: 30px; text-align: center;",
            h4("No statewise plots could be generated"),
            p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
            p("Check the R console for diagnostic messages showing the unique Classification values."),
            p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
          )
        )
      }
      
      shiny::tagList(
        shiny::h2("Statewise Comparison"),
        lapply(snames, function(nm) {
          def <- defaults[["state"]]()
          vul_plot_card_gallery(
            ns = session$ns,      # <-- NAMESPACE FIX
            key = nm,
            label = nm,
            prefix = "state",
            default_w = def$w, default_h = def$h,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_axis_angle = def$axis_angle, default_legend_key_size = def$legend_key_size,
            default_title_hjust = def$title_hjust, default_panel_spacing = def$panel_spacing,
            default_margin_t = def$margin_t, default_margin_r = def$margin_r,
            default_margin_b = def$margin_b, default_margin_l = def$margin_l,
            default_border_lwd = def$border_lwd, default_axis_line_col = def$axis_line_col,
            default_panel_fill = def$panel_fill, default_grid_col = def$grid_col,
            default_border_col = def$border_col, default_col_sfd = def$col_sfd,
            default_col_com = def$col_com, default_col_pen = def$col_pen,
            default_col_cred = def$col_cred, default_axis_text_margin_t = def$axis_text_margin_t,
            default_axis_text_vjust = def$axis_text_vjust
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
        return(
          div(
            class = "info-panel",
            style = "margin-top: 20px; padding: 30px; text-align: center;",
            h4("No percentage-change plots could be generated"),
            p("This usually means the classification labels in your data do not match the expected values in CLASS_LABELS."),
            p("Check the R console for diagnostic messages showing the unique Classification values."),
            p("You may need to update CLASS_LABELS in VulsenAPP_config.R to match your data.")
          )
        )
      }
      
      shiny::tagList(
        shiny::h2("Percentage Change Comparison"),
        lapply(pnames, function(nm) {
          def <- defaults[["pct"]]()
          vul_plot_card_gallery(
            ns = session$ns,      # <-- NAMESPACE FIX
            key = nm,
            label = nm,
            prefix = "pct",
            default_w = def$w, default_h = def$h,
            default_axis_text = def$axis_text, default_axis_title = def$axis_title,
            default_plot_title = def$plot_title, default_strip_text = def$strip_text,
            default_legend_text = def$legend_text, default_legend_title = def$legend_title,
            default_axis_angle = def$axis_angle, default_legend_key_size = def$legend_key_size,
            default_title_hjust = def$title_hjust, default_panel_spacing = def$panel_spacing,
            default_margin_t = def$margin_t, default_margin_r = def$margin_r,
            default_margin_b = def$margin_b, default_margin_l = def$margin_l,
            default_border_lwd = def$border_lwd, default_axis_line_col = def$axis_line_col,
            default_panel_fill = def$panel_fill, default_grid_col = def$grid_col,
            default_border_col = def$border_col, default_col_sfd = def$col_sfd,
            default_col_com = def$col_com, default_col_pen = def$col_pen,
            default_col_cred = def$col_cred, default_axis_text_margin_t = def$axis_text_margin_t,
            default_axis_text_vjust = def$axis_text_vjust
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
      DT::datatable(
        st$obj$final_comp,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        filter = "top"
      )
    })
    
    # -------------------------------------------------------------------------
    # Sample downloads
    # -------------------------------------------------------------------------
    output$download_sample_moody <- shiny::downloadHandler(
      filename = function() basename(sample_moody_path),
      content = function(file) {
        if (!file.exists(sample_moody_path)) {
          stop("Moody's sample file not found: ", sample_moody_path)
        }
        file.copy(sample_moody_path, file, overwrite = TRUE)
      }
    )
    
    output$download_sample_verisk <- shiny::downloadHandler(
      filename = function() basename(sample_verisk_path),
      content = function(file) {
        if (!file.exists(sample_verisk_path)) {
          stop("Verisk sample file not found: ", sample_verisk_path)
        }
        file.copy(sample_verisk_path, file, overwrite = TRUE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Main download handler
    # -------------------------------------------------------------------------
    output$download_selected <- shiny::downloadHandler(
      filename = function() {
        choice <- input$download_type %||% "data"
        ext <- switch(choice,
                      "html" = ".html",
                      "plots" = ".zip",
                      "data" = ".csv",
                      "all" = ".zip",
                      ".zip"
        )
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
          can_use_template <- exists("save_html_report", mode = "function") &&
            file.exists(file.path("templates", "vulsens_report.Rmd"))
          if (can_use_template) {
            save_html_report(obj, file)
          } else {
            save_vulsen_fallback_html(obj, plots, file)
          }
          return(invisible(file))
        }
        
        tmp_dir <- tempfile("vulsen_download_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        
        plot_dir <- file.path(tmp_dir, "plots")
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
        
        plot_files <- c()
        for (group in c("region", "state", "pct")) {
          plot_list <- if (group == "pct") plots[["pct"]] else plots[[group]]
          if (is.null(plot_list)) next
          for (key in names(plot_list)) {
            base_plot <- plot_list[[key]]
            def <- defaults[[group]]()
            override_vals <- list(
              axis_text = input[[paste0(group, "_axis_text_", key)]] %||% def$axis_text %||% 12,
              axis_title = input[[paste0(group, "_axis_title_", key)]] %||% def$axis_title %||% 14,
              plot_title = input[[paste0(group, "_plot_title_", key)]] %||% def$plot_title %||% 16,
              strip_text = input[[paste0(group, "_strip_text_", key)]] %||% def$strip_text %||% 12,
              legend_text = input[[paste0(group, "_legend_text_", key)]] %||% def$legend_text %||% 10,
              legend_title = input[[paste0(group, "_legend_title_", key)]] %||% def$legend_title %||% 10,
              axis_angle = input[[paste0(group, "_axis_angle_", key)]] %||% def$axis_angle %||% 90,
              legend_pos = input[[paste0(group, "_legend_pos_", key)]] %||% def$legend_pos %||% "top",
              legend_show = input[[paste0(group, "_legend_show_", key)]] %||% def$legend_show %||% TRUE,
              legend_key_size = input[[paste0(group, "_legend_key_size_", key)]] %||% def$legend_key_size %||% 0.8,
              title_hjust = input[[paste0(group, "_plot_title_hjust_", key)]] %||% def$title_hjust %||% 0.5,
              axis_line_col = input[[paste0(group, "_axis_line_col_", key)]] %||% def$axis_line_col %||% "black",
              panel_fill = input[[paste0(group, "_panel_fill_", key)]] %||% def$panel_fill %||% "white",
              bg = input[[paste0(group, "_bg_", key)]] %||% def$bg %||% "white",
              grid_col = input[[paste0(group, "_grid_col_", key)]] %||% def$grid_col %||% "#e9ecf3",
              panel_spacing = input[[paste0(group, "_panel_spacing_", key)]] %||% def$panel_spacing %||% 3,
              margin_t = input[[paste0(group, "_plot_margin_t_", key)]] %||% def$margin_t %||% 30,
              margin_r = input[[paste0(group, "_plot_margin_r_", key)]] %||% def$margin_r %||% 10,
              margin_b = input[[paste0(group, "_plot_margin_b_", key)]] %||% def$margin_b %||% 30,
              margin_l = input[[paste0(group, "_plot_margin_l_", key)]] %||% def$margin_l %||% 10,
              border_col = input[[paste0(group, "_panel_border_col_", key)]] %||% def$border_col %||% "black",
              border_lwd = input[[paste0(group, "_panel_border_lwd_", key)]] %||% def$border_lwd %||% 0.5,
              axis_text_margin_t = input[[paste0(group, "_axis_text_margin_t_", key)]] %||% def$axis_text_margin_t %||% 5,
              axis_text_vjust = input[[paste0(group, "_axis_text_vjust_", key)]] %||% def$axis_text_vjust %||% 0.5,
              col_sfd = input[[paste0(group, "_col_sfd_", key)]] %||% def$col_sfd %||% "#6FACDE",
              col_com = input[[paste0(group, "_col_com_", key)]] %||% def$col_com %||% "#F0B323",
              col_pen = input[[paste0(group, "_col_pen_", key)]] %||% def$col_pen %||% "#F0B323",
              col_cred = input[[paste0(group, "_col_cred_", key)]] %||% def$col_cred %||% "#6FACDE"
            )
            p <- vul_apply_overrides(base_plot, input, group, key, override_vals)
            w <- (input[[paste0(group, "_w_", key)]] %||% def$w %||% 9)
            h <- (input[[paste0(group, "_h_", key)]] %||% def$h %||% 5)
            dpi <- (input[[paste0(group, "_dpi_", key)]] %||% def$dpi %||% 150)
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
            htmltools::tags$style(
              htmltools::HTML("
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
              ")
            )
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
















