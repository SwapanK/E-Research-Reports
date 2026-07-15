library(shiny)
library(shinyjs)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER UI — TAB‑BASED LAYOUT, v2 modern redesign
# All input/output IDs are unchanged from the previous version — this is a
# drop-in replacement. Only markup/classes changed (see secmod_style.css).
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

# Helper: lightbulb hint box
sec_hint <- function(text) {
  div(
    class = "sec2-hint",
    span(class = "sec2-hint-icon", icon("lightbulb")),
    span(text)
  )
}

# -----------------------------------------------------------------------------
# Extended override panel for gallery cards (stages 6 & 7, internally s5 & s6)
# Unchanged from previous version — only the CSS classes it references
# have been restyled in secmod_style.css.
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
# Gallery controls – unchanged logic/IDs, restyled via secmod_style.css
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
    sec_hint("Changes take effect after clicking \u2018Apply to all\u2019 below."),
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
      tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),
      tags$script(HTML(sprintf("
        function secmodCartClick(key) {
          Shiny.setInputValue('%s', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler('secmod-dl-state', function(msg) {
            var el = document.getElementById(msg.id);
            if (!el) return;
            el.classList.remove('state-locked', 'state-ready', 'state-active');
            el.classList.add(msg.state);
          });
        });
      ", ns("sec_cart_click"))))
    ),
    
    div(
      class = "single-scroll-panel",
      
      # ===== HEADER =====
      div(
        class = "sec2-header",
        div(class = "sec2-header-eyebrow", icon("shield-halved"), "MODEL TESTING \u00b7 SECONDARY MODIFIER"),
        h1("Secondary Modifier"),
        p("Work through the pipeline stage by stage \u2014 prepare data, analyze sensitivity, and export a polished report at any checkpoint.")
      ),
      
      # ===== ASSESSMENT PROGRESS PANEL (rendered server-side) =====
      uiOutput(ns("sec_stepper")),
      
      # ===== TABBED WIZARD (8 stages) =====
      tabsetPanel(
        id = ns("sec_tabs"),
        type = "tabs",
        
        # ----- STAGE 1 — LOAD DATA --------------------------------------------
        tabPanel(
          title = sec_tab_title(1, "Load data"), value = "stage1",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Upload your data & pick colours"),
              uiOutput(ns("sec_stage1_status"), inline = TRUE)
            ),
            sec_hint("Upload aal_State and aal_USA (.rds or .csv), choose whether to use the built‑in modifier mapping, and set the SFD / COM / Penalty / Credit colours used throughout every chart and report."),
            # --- File inputs (stylist) ---
            fluidRow(
              column(4, div(class = "sec2-file-field",
                            fileInput(ns("sec_file_state"), "aal_State (.rds or .csv)", accept = c(".rds", ".csv")))),
              column(4, div(class = "sec2-file-field",
                            fileInput(ns("sec_file_usa"), "aal_USA (.rds or .csv)", accept = c(".rds", ".csv")))),
              column(4,
                     checkboxInput(ns("sec_use_default_mapping"), "Use built-in modifier mapping", value = TRUE),
                     conditionalPanel(
                       condition = sprintf("!input['%s']", ns("sec_use_default_mapping")),
                       div(class = "sec2-file-field",
                           fileInput(ns("sec_file_mapping"), "SecMod_name mapping (.csv)", accept = ".csv"))
                     )
              )
            ),
            tags$hr(),
            # --- Colour swatches (stylist) ---
            fluidRow(
              column(3, div(class = "sec2-swatch-field",
                            colourInput(ns("sec_color_sfd"), "SFD color", value = "#6FACDE", showColour = "text"))),
              column(3, div(class = "sec2-swatch-field",
                            colourInput(ns("sec_color_com"), "COM color", value = "#F0B323", showColour = "text"))),
              column(3, div(class = "sec2-swatch-field",
                            colourInput(ns("sec_color_max"), "Penalty color", value = "#F0B323", showColour = "text"))),
              column(3, div(class = "sec2-swatch-field",
                            colourInput(ns("sec_color_min"), "Credit color", value = "#6FACDE", showColour = "text")))
            ),
            div(
              style = "margin-top: 30px; text-align: center;",
              actionButton(ns("sec_load"), "Load data", icon = icon("upload"), class = "btn-glass btn-glass-lg")
            )
          )
        ),
        
        # ----- STAGE 2 — BUILD TABLES ------------------------------------------
        tabPanel(
          title = sec_tab_title(2, "Build tables"), value = "stage2",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "finaltable() / finaltable_allUSA(), or upload pre-built CSVs"),
              uiOutput(ns("sec_stage2_status"), inline = TRUE)
            ),
            sec_hint("You can skip straight to this stage by uploading previously downloaded aal_final / aal_final_USA CSVs below."),
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
            # --- Skip uploads (stylist) ---
            fluidRow(
              column(6, div(class = "sec2-file-field",
                            fileInput(ns("sec_upload_final"), "Upload aal_final.csv (skip stage 1+2)", accept = ".csv"))),
              column(6, div(class = "sec2-file-field",
                            fileInput(ns("sec_upload_final_usa"), "Upload aal_final_USA.csv (skip stage 1+2)", accept = ".csv")))
            ),
            uiOutput(ns("sec_stage2_summary"))
          )
        ),
        
        # ----- STAGE 3 — MIN/MAX SUMMARY ---------------------------------------
        tabPanel(
          title = sec_tab_title(3, "Min / max"), value = "stage3",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Compute state and USA min/max ranges"),
              uiOutput(ns("sec_stage3_status"), inline = TRUE)
            ),
            sec_hint("This works out the minimum and maximum relative loss cost per state and line of business, which every later chart uses to scale correctly."),
            actionButton(ns("sec_minmax"), "Compute min / max", icon = icon("calculator"), class = "btn-glass"),
            tags$hr(),
            div(class = "sec-table-wrap", tableOutput(ns("sec_minmax_tbl")))
          )
        ),
        
        # ----- STAGE 4 — DOWNLOAD DEFAULT RESULTS ------------------------------
        tabPanel(
          title = sec_tab_title(4, "Download default results"), value = "stage4",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Generate a default HTML report (no customizations)"),
              uiOutput(ns("sec_stage4_status"), inline = TRUE)
            ),
            sec_hint("Unlocks once Stage 3 (Min / max) is complete. This knits the default template with your currently loaded data and colours \u2014 a good baseline before customizing in Stages 5\u20137."),
            div(
              class = "sec2-report-flow",
              actionButton(
                ns("sec_generate_default"),
                "Generate default report",
                icon = icon("file-arrow-down"),
                class = "sec2-flow-btn"
              ),
              span(class = "report-arrow sec2-flow-arrow", icon("arrow-right")),
              downloadButton(
                ns("sec_dl_default"),
                "Download default HTML",
                class = "report-download-btn sec2-dl-btn state-locked",
                icon = NULL
              ),
              div(class = "sec2-flow-caption", "Locked until Stage 3 is complete \u2014 then generate to unlock the download.")
            ),
            div(class = "sec-status-message", uiOutput(ns("sec_stage4_message")))
          )
        ),
        
        # ----- STAGE 5 — CREDIT / PENALTY --------------------------------------
        tabPanel(
          title = sec_tab_title(5, "Credit / penalty"), value = "stage5",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Credit / penalty sensitivity chart"),
              uiOutput(ns("sec_stage5_status"), inline = TRUE)
            ),
            sec_hint("Shows which modifiers move loss cost by more than 10% — split into credits (below 1.0) and penalties (above 1.0)."),
            actionButton(ns("sec_credit"), "Generate credit / penalty chart", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_credit_card"))
          )
        ),
        
        # ----- STAGE 6 — STATE SENSITIVITY -------------------------------------
        tabPanel(
          title = sec_tab_title(6, "State sensitivity"), value = "stage6",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "State‑by‑state sensitivity charts"),
              uiOutput(ns("sec_stage6_status"), inline = TRUE)
            ),
            sec_hint("Builds one chart per state comparing SFD vs COM — this renders every state individually, so it can take a little while."),
            actionButton(ns("sec_state_plots"), "Generate all state plots", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_stage6_gallery_controls")),
            uiOutput(ns("sec_stage6_gallery"))
          )
        ),
        
        # ----- STAGE 7 — MODIFIER DETAIL ---------------------------------------
        tabPanel(
          title = sec_tab_title(7, "Modifier detail"), value = "stage7",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Modifier detail charts"),
              uiOutput(ns("sec_stage7_status"), inline = TRUE)
            ),
            sec_hint("Builds one chart per modifier category (e.g. Residential Exterior) comparing SFD vs COM across every state."),
            actionButton(ns("sec_modifier_plots"), "Generate all modifier plots", icon = icon("play"), class = "btn-glass"),
            tags$hr(),
            uiOutput(ns("sec_stage7_gallery_controls")),
            uiOutput(ns("sec_stage7_gallery"))
          )
        ),
        
        # ----- STAGE 8 — DOWNLOAD CUSTOMIZED RESULTS ---------------------------
        tabPanel(
          title = sec_tab_title(8, "Download customized results"), value = "stage8",
          div(
            class = "sec2-panel",
            div(
              class = "sec2-panel-top",
              span(class = "sec2-panel-kicker", "Generate a final HTML report with all your customizations from stages 5\u20137"),
              uiOutput(ns("sec_stage8_status"), inline = TRUE)
            ),
            sec_hint("Unlocks once Stages 5, 6 and 7 are all complete. The final report includes every plot with your personal overrides \u2014 colours, sizes, fonts, margins and more."),
            div(
              class = "sec2-report-flow",
              actionButton(
                ns("sec_generate_customized"),
                "Generate customized report",
                icon = icon("file-arrow-down"),
                class = "sec2-flow-btn"
              ),
              span(class = "report-arrow sec2-flow-arrow", icon("arrow-right")),
              downloadButton(
                ns("sec_dl_customized"),
                "Download customized HTML",
                class = "report-download-btn sec2-dl-btn state-locked",
                icon = NULL
              ),
              div(class = "sec2-flow-caption", "Locked until Stages 5, 6 & 7 are complete \u2014 then generate to unlock the download.")
            ),
            div(class = "sec-status-message", uiOutput(ns("sec_stage8_message")))
          )
        )
      ),
      
      br()
    )
  )
}


