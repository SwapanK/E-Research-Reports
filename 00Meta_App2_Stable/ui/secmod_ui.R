library(shiny)
library(shinyjs)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER UI — TAB‑BASED LAYOUT (like the Vulnerability page)
# =============================================================================

# Helper: one tab title with a live status badge
sec_tab_title <- function(number, title) {
  tagList(
    span(
      style = "display:inline-flex; align-items:center; justify-content:center;
                width:20px; height:20px; border-radius:50%; background:rgba(255,255,255,0.25);
                font-size:11px; font-weight:700; margin-right:6px;",
      number
    ),
    title
  )
}

# -----------------------------------------------------------------------------
# Extended override panel for gallery cards (stages 6 & 7, internally s5 & s6)
# Now accepts a namespace function `ns` to generate namespaced IDs.
# -----------------------------------------------------------------------------
sec_plot_card_gallery <- function(ns, key, label, prefix,
                                  default_w = 9, default_h = 5,
                                  default_axis_text = 12, default_axis_title = 14,
                                  default_plot_title = 16, default_strip_text = 12,
                                  default_legend_text = 10, default_legend_title = 10,
                                  default_axis_angle = 90, default_legend_key_size = 0.8,
                                  default_title_hjust = 0.5, default_panel_spacing = 3,
                                  default_margin_t = 30, default_margin_r = 10,
                                  default_margin_b = 30, default_margin_l = 10,
                                  default_border_lwd = 0.5,
                                  default_axis_line_col = "black", default_panel_fill = "white",
                                  default_grid_col = "#e9ecf3", default_border_col = "black",
                                  default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                                  default_col_pen = "#F0B323", default_col_cred = "#6FACDE") {
  ov_id <- ns(paste0(prefix, "_override_", key))
  
  div(
    class = "cart-item-card sec-plot-card",
    style = "margin-bottom:14px;",
    
    div(
      class = "cart-item-header",
      span(class = "cart-item-badge", label),
      div(
        style = "display:flex; gap:4px;",
        tags$button(
          onclick = sprintf("$('#%s').toggleClass('sec-open');", ov_id),
          class = "btn-icon-cart", title = "Adjust size, colours, text & legend",
          icon("sliders-h")
        ),
        downloadButton(
          outputId = ns(paste0(prefix, "_dl_", key)),
          label = NULL,
          icon = icon("download"),
          class = "btn-icon-cart",
          title = "Download"
        ),
        tags$button(
          onclick = sprintf("secmodCartClick('%s|%s')", prefix, key),
          class = "btn-icon-cart", title = "Add to cart",
          icon("cart-plus")
        )
      )
    ),
    
    div(
      class = "sec-plot-frame",
      uiOutput(ns(paste0(prefix, "_plot_frame_", key)))
    ),
    
    div(
      id = ov_id, class = "sec-override-panel",
      numericInput(ns(paste0(prefix, "_w_", key)), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_h_", key)), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_axis_text_", key)), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_axis_title_", key)), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_title_", key)), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_strip_text_", key)), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_legend_text_", key)), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_legend_title_", key)), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_axis_angle_", key)), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_legend_key_size_", key)), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_title_hjust_", key)), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
      numericInput(ns(paste0(prefix, "_panel_spacing_", key)), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_margin_t_", key)), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_margin_r_", key)), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_margin_b_", key)), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_plot_margin_l_", key)), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
      selectInput(ns(paste0(prefix, "_legend_pos_", key)), "Legend",
                  choices = c("top", "bottom", "left", "right", "none"),
                  selected = "top", width = "80px"),
      checkboxInput(ns(paste0(prefix, "_legend_show_", key)), "Show legend", value = TRUE),
      colourInput(ns(paste0(prefix, "_axis_line_col_", key)), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_panel_fill_", key)), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_grid_col_", key)), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_panel_border_col_", key)), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
      numericInput(ns(paste0(prefix, "_panel_border_lwd_", key)), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px"),
      colourInput(ns(paste0(prefix, "_col_sfd_", key)), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_col_com_", key)), "COM", value = default_col_com, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_col_pen_", key)), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_col_cred_", key)), "Credit", value = default_col_cred, showColour = "text", width = "80px")
    )
  )
}

# -----------------------------------------------------------------------------
# Gallery controls – accepts ns and uses it for all IDs
# (Used for stages 6 & 7, internally s5 & s6)
# -----------------------------------------------------------------------------
sec_gallery_controls_ui <- function(ns, prefix, n, default_w, default_h, default_dpi,
                                    default_axis_text = 12, default_axis_title = 14,
                                    default_plot_title = 16, default_strip_text = 12,
                                    default_legend_text = 10, default_legend_title = 10,
                                    default_axis_angle = 90, default_legend_pos = "top",
                                    default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                                    default_col_pen = "#F0B323", default_col_cred = "#6FACDE",
                                    default_legend_key_size = 0.8, default_title_hjust = 0.5,
                                    default_axis_line_col = "black", default_panel_fill = "white",
                                    default_grid_col = "#e9ecf3",
                                    default_panel_spacing = 3, default_margin_t = 30,
                                    default_margin_r = 10, default_margin_b = 30, default_margin_l = 10,
                                    default_border_col = "black", default_border_lwd = 0.5,
                                    default_bg = "white") {
  div(
    class = "sec-gallery-bar",
    div(
      class = "sec-gallery-bar-head",
      div(class = "sec-gallery-bar-title", icon("sliders-h"), "Gallery defaults"),
      span(class = "sec-gallery-bar-count", paste(n, "plots"))
    ),
    # Hint: changes take effect only on apply
    div(
      style = "font-size:11px; color:#718096; margin-bottom:8px; display:flex; align-items:center; gap:6px;",
      icon("lightbulb", class = "text-warning"), 
      " Changes take effect after clicking 'Apply to all' below."
    ),
    div(
      class = "sec-gallery-bar-grid",
      numericInput(ns(paste0(prefix, "_default_w")), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_h")), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_dpi")), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_axis_text")), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_axis_title")), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_plot_title")), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_strip_text")), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_legend_text")), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_legend_title")), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_axis_angle")), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_legend_key_size")), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_title_hjust")), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_panel_spacing")), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_margin_t")), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_margin_r")), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_margin_b")), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(ns(paste0(prefix, "_default_margin_l")), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
      selectInput(ns(paste0(prefix, "_default_legend_pos")), "Legend", choices = c("top","bottom","left","right","none"), selected = default_legend_pos, width = "80px"),
      selectInput(ns(paste0(prefix, "_default_bg")), "Background", choices = c("White" = "white", "Transparent" = "transparent"), selected = default_bg, width = "100px"),
      colourInput(ns(paste0(prefix, "_default_axis_line_col")), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_panel_fill")), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_grid_col")), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_border_col")), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
      numericInput(ns(paste0(prefix, "_default_border_lwd")), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px"),
      colourInput(ns(paste0(prefix, "_default_col_sfd")), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_col_com")), "COM", value = default_col_com, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_col_pen")), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
      colourInput(ns(paste0(prefix, "_default_col_cred")), "Credit", value = default_col_cred, showColour = "text", width = "80px")
    ),
    div(
      class = "sec-gallery-bar-foot",
      actionButton(ns(paste0(prefix, "_apply_all")), paste0("Apply to all ", n, " plots"),
                   icon = icon("wand-magic-sparkles"), class = "btn-glass")
    )
  )
}

# =============================================================================
# MAIN MODULE UI — TAB‑BASED LAYOUT
# =============================================================================
secmod_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(HTML(sprintf("
        function secmodCartClick(key) {
          Shiny.setInputValue('%s', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
      ", ns("sec_cart_click")))),
      # Additional CSS for Stage 4 & 8 flow
      tags$style(HTML("
        .report-flow {
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 16px;
          margin: 10px 0;
          flex-wrap: wrap;
        }
        .report-flow .btn {
          flex: 0 1 auto;
        }
        .report-arrow {
          font-size: 28px;
          color: #d1d5db;
          transition: color 0.3s ease;
          line-height: 1;
        }
        .report-arrow.active {
          color: #0075BC;
        }
        .report-download-btn {
          transition: all 0.3s ease;
        }
        .report-download-btn.active {
          background: #0075BC !important;
          border-color: #0075BC !important;
          color: white !important;
        }
        .report-download-btn:not(.active) {
          background: #f3f4f6 !important;
          border-color: #d1d5db !important;
          color: #6b7280 !important;
        }
        .hint-text {
          display: flex;
          align-items: center;
          gap: 8px;
          font-size: 13px;
          color: #6b7280;
          background: #f3f6fa;
          padding: 8px 14px;
          border-radius: 8px;
          margin: 6px 0 12px 0;
        }
        .hint-text i {
          color: #f59e0b;
          font-size: 16px;
        }
        .download-group {
          display: flex;
          gap: 10px;
          flex-wrap: wrap;
          align-items: center;
        }
        .download-group .btn {
          margin: 0;
        }
        /* Style for stage 2 download buttons */
        .btn-download-csv {
          background: rgba(0, 117, 188, 0.08) !important;
          border: 1px solid rgba(0, 117, 188, 0.25) !important;
          color: #0075BC !important;
          font-weight: 600 !important;
          border-radius: 6px !important;
          padding: 6px 16px !important;
          transition: all 0.2s;
        }
        .btn-download-csv:hover {
          background: rgba(0, 117, 188, 0.18) !important;
          border-color: #0075BC !important;
        }
        /* Progress chip lock/open styles */
        .vulsen-progress-chip.is-locked {
          opacity: 0.6;
          background: #e5e7eb;
          color: #6b7280;
        }
        .vulsen-progress-chip.is-locked i {
          color: #9ca3af;
        }
        .vulsen-progress-chip.is-done i {
          color: #2f7d5c;
        }
      "))
    ),
    
    div(
      class = "single-scroll-panel",
      
      # HEADER
      div(
        class = "app-header-plain",
        h1("Secondary Modifier"),
        p("Work through the secondary modifier pipeline stage by stage: load data, build tables, summarize, and generate every plot with its own size and cart controls.")
      ),
      
      # PROGRESS STRIP (updated to 8 stages with lock/unlock)
      uiOutput(ns("sec_stepper")),
      
      br(),
      
      # TABBED WIZARD (8 stages)
      tabsetPanel(
        id = ns("sec_tabs"),
        type = "tabs",
        
        # ----- STAGE 1 — LOAD DATA --------------------------------------------
        tabPanel(
          title = sec_tab_title(1, "Load data"), value = "stage1",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "aal_State, aal_USA, modifier mapping, type colors"),
              uiOutput(ns("sec_stage1_status"), inline = TRUE)
            ),
            tags$hr(),
            fluidRow(
              column(4, fileInput(ns("sec_file_state"), "aal_State (.rds or .csv)", accept = c(".rds", ".csv"))),
              column(4, fileInput(ns("sec_file_usa"), "aal_USA (.rds or .csv)", accept = c(".rds", ".csv"))),
              column(4,
                     checkboxInput(ns("sec_use_default_mapping"), "Use built-in modifier mapping", value = TRUE),
                     conditionalPanel(
                       condition = sprintf("!input['%s']", ns("sec_use_default_mapping")),
                       fileInput(ns("sec_file_mapping"), "SecMod_name mapping (.csv)", accept = ".csv")
                     )
              )
            ),
            tags$hr(),
            fluidRow(
              column(3, colourInput(ns("sec_color_sfd"), "SFD color", value = "#6FACDE", showColour = "text")),
              column(3, colourInput(ns("sec_color_com"), "COM color", value = "#F0B323", showColour = "text")),
              column(3, colourInput(ns("sec_color_max"), "Penalty color", value = "#F0B323", showColour = "text")),
              column(3, colourInput(ns("sec_color_min"), "Credit color", value = "#6FACDE", showColour = "text"))
            ),
            div(actionButton(ns("sec_load"), "Load data", icon = icon("upload"), class = "btn-glass"))
          )
        ),
        
        # ----- STAGE 2 — BUILD TABLES ------------------------------------------
        tabPanel(
          title = sec_tab_title(2, "Build tables"), value = "stage2",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "finaltable() / finaltable_allUSA(), or upload pre-built CSVs"),
              uiOutput(ns("sec_stage2_status"), inline = TRUE)
            ),
            tags$hr(),
            p(class = "commentary-text", "Either build from the stage 1 data, or skip straight here by uploading previously downloaded aal_final / aal_final_USA CSVs."),
            fluidRow(
              column(6, actionButton(ns("sec_build"), "Build tables from loaded data", icon = icon("cogs"), class = "btn-glass")),
              column(6,
                     div(class = "download-group",
                         downloadButton(ns("sec_dl_final"), "Download aal_final.csv", class = "btn-download-csv"),
                         downloadButton(ns("sec_dl_final_usa"), "Download aal_final_USA.csv", class = "btn-download-csv")
                     )
              )
            ),
            tags$hr(),
            fluidRow(
              column(6, fileInput(ns("sec_upload_final"), "Upload aal_final.csv (skip stage 1+2)", accept = ".csv")),
              column(6, fileInput(ns("sec_upload_final_usa"), "Upload aal_final_USA.csv (skip stage 1+2)", accept = ".csv"))
            ),
            uiOutput(ns("sec_stage2_summary"))
          )
        ),
        
        # ----- STAGE 3 — MIN/MAX SUMMARY ---------------------------------------
        tabPanel(
          title = sec_tab_title(3, "Min / max"), value = "stage3",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "STATEminmax(), Countryminmax(), CountryminmaxTable()"),
              uiOutput(ns("sec_stage3_status"), inline = TRUE)
            ),
            tags$hr(),
            actionButton(ns("sec_minmax"), "Compute min / max", icon = icon("calculator"), class = "btn-glass"),
            tags$hr(),
            div(class = "sec-table-wrap", tableOutput(ns("sec_minmax_tbl")))
          )
        ),
        
        # ----- STAGE 4 — DOWNLOAD DEFAULT RESULTS (NEW) ------------------------
        tabPanel(
          title = sec_tab_title(4, "Download default results"), value = "stage4",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "Generate a default HTML report (no customizations)"),
              uiOutput(ns("sec_stage4_status"), inline = TRUE)
            ),
            tags$hr(),
            p(class = "commentary-text",
              "This stage knits the default template with the currently loaded data and colours, producing a self-contained HTML report.
               Use this as a baseline before applying custom overrides in the following stages."
            ),
            # Flow with arrow and dynamic download button (same as stage 8)
            div(
              class = "report-flow",
              actionButton(
                ns("sec_generate_default"),
                "Generate default report",
                icon = icon("file-arrow-down"),
                class = "btn-glass",
                style = "flex:0 1 auto;"
              ),
              span(class = "report-arrow", icon("arrow-right")),
              downloadButton(
                ns("sec_dl_default"),
                "Download default HTML",
                class = "report-download-btn",  # base class; active toggled by server
                icon = icon("download")
              )
            ),
            tags$hr(),
            div(
              class = "sec-status-message",
              uiOutput(ns("sec_stage4_message"))
            )
          )
        ),
        
        # ----- STAGE 5 — CREDIT / PENALTY (old stage 4) -----------------------
        tabPanel(
          title = sec_tab_title(5, "Credit / penalty"), value = "stage5",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "Credit_Penalty() — modifiers with >10% sensitivity"),
              uiOutput(ns("sec_stage5_status"), inline = TRUE)
            ),
            tags$hr(),
            actionButton(ns("sec_credit"), "Generate credit / penalty chart", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_credit_card"))   # this uses internal s4 prefix but displays in stage 5
          )
        ),
        
        # ----- STAGE 6 — STATE SENSITIVITY (old stage 5) ----------------------
        tabPanel(
          title = sec_tab_title(6, "State sensitivity"), value = "stage6",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "STATE_plot() for every state and LOB, rendered one by one"),
              uiOutput(ns("sec_stage6_status"), inline = TRUE)
            ),
            tags$hr(),
            actionButton(ns("sec_state_plots"), "Generate all state plots", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_stage6_gallery_controls")),
            uiOutput(ns("sec_stage6_gallery"))
          )
        ),
        
        # ----- STAGE 7 — MODIFIER DETAIL (old stage 6) ------------------------
        tabPanel(
          title = sec_tab_title(7, "Modifier detail"), value = "stage7",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "indmod() for every modifier, rendered one by one"),
              uiOutput(ns("sec_stage7_status"), inline = TRUE)
            ),
            tags$hr(),
            actionButton(ns("sec_modifier_plots"), "Generate all modifier plots", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_stage7_gallery_controls")),
            uiOutput(ns("sec_stage7_gallery"))
          )
        ),
        
        # ----- STAGE 8 — DOWNLOAD CUSTOMIZED RESULTS (NEW) --------------------
        tabPanel(
          title = sec_tab_title(8, "Download customized results"), value = "stage8",
          div(
            class = "glass-card sec-tab-panel",
            div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              tags$small(style = "color:#718096;", "Generate a final HTML report with all your customizations from stages 5–7"),
              uiOutput(ns("sec_stage8_status"), inline = TRUE)
            ),
            tags$hr(),
            p(class = "commentary-text",
              "Once you have generated and customized plots in Stages 5, 6, and 7, click below to produce a single HTML report
               that includes every plot with your personal overrides (colours, sizes, fonts, margins, etc.)."
            ),
            # Flow with arrow and dynamic download button
            div(
              class = "report-flow",
              actionButton(
                ns("sec_generate_customized"),
                "Generate customized report",
                icon = icon("file-arrow-down"),
                class = "btn-glass",
                style = "flex:0 1 auto;"
              ),
              span(class = "report-arrow", icon("arrow-right")),
              downloadButton(
                ns("sec_dl_customized"),
                "Download customized HTML",
                class = "report-download-btn",
                icon = icon("download")
              )
            ),
            tags$hr(),
            div(
              class = "sec-status-message",
              uiOutput(ns("sec_stage8_message"))
            )
          )
        )
      ),
      
      br()
    )
  )
}


