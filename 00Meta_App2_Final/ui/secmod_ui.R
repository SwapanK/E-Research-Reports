library(shiny)
library(shinyjs)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER UI — TWO‑COLUMN LAYOUT (gold standard style)
# =============================================================================

# Helper: one tab title with a live status badge (used in right panel tabs)
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

# Helper: lightbulb hint box (used inside stage cards)
sec_hint <- function(text) {
  div(
    class = "sec2-hint",
    span(class = "sec2-hint-icon", icon("lightbulb")),
    span(text)
  )
}

# -----------------------------------------------------------------------------
# Extended override panel for gallery cards (stages 5 & 6, internally s5 & s6)
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
          class = "btn-icon-cart sec2-icon-btn", title = "Adjust size, colours, text & legend",
          icon("sliders-h")
        ),
        downloadButton(
          outputId = ns(paste0(prefix, "_dl_", key)),
          label = NULL,
          icon = icon("download"),
          class = "btn-icon-cart sec2-icon-btn",
          title = "Download"
        ),
        tags$button(
          onclick = sprintf("secmodCartClick('%s|%s')", prefix, key),
          class = "btn-icon-cart sec2-icon-btn", title = "Add to cart",
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
    sec_hint("Changes take effect after clicking ‘Apply to all’ below."),
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
                   icon = icon("wand-magic-sparkles"), class = "sec2-btn")
    )
  )
}

# =============================================================================
# MAIN MODULE UI — TWO‑COLUMN LAYOUT (gold standard style)
# =============================================================================
secmod_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),
      
      # ---- Inline CSS for progress stepper ----
      tags$style(HTML("
        .sec-step-clickable {
          cursor: pointer;
          transition: transform 0.15s ease;
        }
        .sec-step-clickable:hover {
          transform: translateY(-1px);
        }
        .sec-step-circle {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 28px;
          height: 28px;
          border-radius: 50%;
          font-weight: 700;
          font-size: 13px;
          color: #fff;
        }
        .sec-step-circle.done {
          background: #4AA57F;
        }
        .sec-step-circle.pending {
          background: #e2e8f0;
          color: #4a5568;
        }
        .sec-step-label {
          font-size: 9px;
          font-weight: 600;
          color: #4a5568;
          text-align: center;
          margin-top: 2px;
          white-space: nowrap;
        }
        .sec-connector {
          flex: 1;
          height: 2px;
          margin: 0 2px;
        }
        .sec-connector.done {
          background: #4AA57F;
        }
        .sec-connector.pending {
          background: #e2e8f0;
        }
      ")),
      
      # ---- JavaScript for scrolling ----
      tags$script(HTML(sprintf("
        function scrollToSection(id) {
          var panel = document.querySelector('.left-scroll-panel');
          var target = document.getElementById(id);
          if (panel && target) {
            panel.scrollTo({
              top: target.offsetTop - 20,
              behavior: 'smooth'
            });
          }
        }
      "))),
      
      # ---- Existing scripts (cart click, colour sync, etc.) ----
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

        // ---- Colour swatch fields: keep the left accent strip in sync ----
        function secmodSyncSwatch(el) {
          var wrap = el.closest('.sec2-swatch-field');
          if (!wrap) return;
          var val = el.value || el.getAttribute('value');
          if (val) wrap.style.setProperty('--swatch-color', val);
        }
        function secmodSyncAllSwatches() {
          document.querySelectorAll('.sec2-swatch-field input.shiny-colour-input')
            .forEach(secmodSyncSwatch);
        }

        $(document).on('shiny:connected', function() {
          secmodSyncAllSwatches();
          $(document).on('input change', '.sec2-swatch-field input.shiny-colour-input', function() {
            secmodSyncSwatch(this);
          });
        });

        setTimeout(secmodSyncAllSwatches, 500);
      ", ns("sec_cart_click"))))
    ),
    
    div(
      class = "app-shell",
      
      # ===== HEADER =====
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Secondary Modifier"),
          img(src = "GallagherRe_StackedLarge-3D.png", class = "header-logo")
        ),
        div(
          class = "header-sub",
          p("Work through the pipeline stage by stage — prepare data, analyze sensitivity, and export a polished report at any checkpoint.")
        )
      ),
      
      # ===== BODY =====
      div(
        class = "app-body",
        
        # ----- LEFT PANEL (scrollable controls) -----
        div(
          class = "left-scroll-panel",
          
          # ---- Section 1: Model and Exposure Settings ----
          div(
            id = "sec-settings-card",   # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("1. Model and Exposure Settings"),
              uiOutput(ns("sec_settings_status"), inline = TRUE)
            ),
            selectInput(
              ns("sec_vendor"), 
              "Vendor model family",
              choices = c("Moody's", "Verisk"),
              selected = "Moody's",
              width = "100%",
              selectize = FALSE
            ),
            textInput(ns("sec_country"), "Country code", value = "US", width = "100%"),
            selectInput(
              ns("sec_peril"), 
              "Peril",
              choices = names(peril_lookup()),
              selected = "SCS",
              width = "100%",
              selectize = FALSE
            ),
            uiOutput(ns("sec_subperil_ui")),
            textInput(ns("sec_suffix"), "Suffix", value = "2026", width = "100%")
          ),
          
          # ---- Section 2: Inputs ----
          div(
            id = "sec-inputs-card",     # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("2. Inputs"),
              uiOutput(ns("sec_stage1_status"), inline = TRUE)
            ),
            sec_hint("Upload State AAL, Country AAL and the SecMod mapping file (.rds or .csv). All three are required."),
            
            # File inputs (default Shiny styling)
            fileInput(ns("sec_file_state"), "Load State AAL (.rds or .csv)", accept = c(".rds", ".csv"), width = "100%"),
            fileInput(ns("sec_file_usa"), "Load Country AAL (.rds or .csv)", accept = c(".rds", ".csv"), width = "100%"),
            fileInput(ns("sec_file_mapping"), "Load SecMod File (.csv)", accept = ".csv", width = "100%"),
            
            # ---- Sample download buttons: PLAIN DEFAULT SHINY BUTTONS ----
            div(
              style = "display: flex; flex-direction: column; gap: 6px; width: 100%;",
              downloadButton(ns("sec_dl_sample_state"), "Download sample State AAL (.csv)", class = "btn-default"),
              downloadButton(ns("sec_dl_sample_country"), "Download sample Country AAL (.csv)", class = "btn-default"),
              downloadButton(ns("sec_dl_sample_mapping"), "Download sample SecMod File (.csv)", class = "btn-default")
            ),
            
            tags$hr(class = "soft"),
            
            # ---- Colour pickers: PLAIN SHINY DEFAULTS ----
            colourInput(ns("sec_color_sfd"), "SFD color", value = "#6FACDE", showColour = "both"),
            colourInput(ns("sec_color_com"), "COM color", value = "#F0B323", showColour = "both"),
            colourInput(ns("sec_color_max"), "Penalty color", value = "#F0B323", showColour = "both"),
            colourInput(ns("sec_color_min"), "Credit color", value = "#6FACDE", showColour = "both"),
            
            tags$hr(class = "soft"),
            
            # Analyse Input button
            actionButton(
              ns("sec_analyse"), 
              "Analyse Input", 
              icon = icon("play"), 
              class = "create-btn",
              style = "width:100%; margin-top:6px;"
            ),
            
            # Status summary (optional)
            uiOutput(ns("sec_stage2_summary"))
          ),
          
          # ---- Section 3: Credit / penalty ----
          div(
            id = "sec-credit-card",     # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("3. Credit / penalty"),
              uiOutput(ns("sec_stage4_status"), inline = TRUE)
            ),
            sec_hint("Shows which modifiers move loss cost by more than 10% — split into credits (below 1.0) and penalties (above 1.0)."),
            actionButton(ns("sec_credit"), "Generate Credit / penalty plot", icon = icon("play"), class = "primary-btn")
          ),
          
          # ---- Section 4: State sensitivity ----
          div(
            id = "sec-state-card",      # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("4. State sensitivity"),
              uiOutput(ns("sec_stage5_status"), inline = TRUE)
            ),
            sec_hint("Builds one chart per state comparing SFD vs COM — this renders every state individually, so it can take a little while."),
            actionButton(ns("sec_state_plots"), "Generate all State plots", icon = icon("play"), class = "primary-btn")
          ),
          
          # ---- Section 5: Individual Modifier ----
          div(
            id = "sec-modifier-card",   # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("5. Individual Modifier"),
              uiOutput(ns("sec_stage6_status"), inline = TRUE)
            ),
            sec_hint("Builds one chart per modifier category (e.g. Residential Exterior) comparing SFD vs COM across every state."),
            actionButton(ns("sec_modifier_plots"), "Generate all modifiers plots", icon = icon("play"), class = "primary-btn")
          ),
          
          # ---- Section 6: Report ----
          div(
            id = "sec-report-card",     # <-- ID for scrolling
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("6. Report"),
              uiOutput(ns("sec_stage7_status"), inline = TRUE)
            ),
            sec_hint("Generate plots for Stages 4 to 6 → Generate Report → Download the customized report."),
            div(
              style = "display: flex; flex-direction: column; gap: 8px;",
              actionButton(
                ns("sec_generate_customized"),
                "Generate Report",
                icon = icon("file-lines"),
                class = "create-btn"
              ),
              downloadButton(
                ns("sec_dl_customized"),
                "Download Report",
                class = "primary-btn state-locked"
              )
            )
          )
        ),
        
        # ----- RIGHT PANEL (output and info) -----
        div(
          class = "right-panel",
          
          # ---- Status ----
          uiOutput(ns("status_ui")),
          
          # ---- About this section ----
          div(
            class = "info-panel",
            h3("About this section"),
            p("This article compares various ", strong("secondary modifiers"), " in the RMS HDv1 US-SCS model, considering all perils combined (hail, tornado, and straight-line winds). Secondary modifiers are presented in a ", strong("credit-penalty"), " format. ", em("Credit"), " implies that a particular modifier contributes to a ", em("reduction"), " in average annual loss (AAL), relative to the ", em("unknown"), " case, expressed in percentage terms, whereas ", em("penalty"), " implies that a modifier contributes to an ", em("increase"), " in AAL. The following three exhibits are presented:"),
            tags$ul(
              tags$li(strong("Location selection for secondary modifier:"), " This section explains how the locations where selected for performing the secondary modifier sensitivity."),
              tags$li(strong("Country-wide impact:"),
                      tags$ul(
                        tags$li("i) Table showing the detailed impact of all secondary modifiers on COM and SFD occupancy."),
                        tags$li("ii) Figure showing the impact of only those secondary modifiers whose impact is greater than 10%.")
                      )
              ),
              tags$li(strong("State-wide impact:"), " Impact of secondary modifiers by state for SFD and COM occupancy."),
              tags$li(strong("Impact by individual modifiers:"), " Impact of each secondary modifier by state, for example, how different types of ", em("roof covering"), " affect relative AAL compared to the case with no secondary modifiers.")
            )
          ),
          
          # ---- Pipeline progress stepper ----
          uiOutput(ns("sec_progress_panel")),
          
          # ---- Output tabs ----
          tabsetPanel(
            id = ns("sec_output_tabs"),
            
            tabPanel(
              title = "Min / max Table",
              br(),
              div(class = "sec-table-wrap", DTOutput(ns("sec_minmax_tbl")))
            ),
            
            tabPanel(
              title = "Credit / penalty",
              br(),
              uiOutput(ns("sec_credit_card"))
            ),
            
            tabPanel(
              title = "State sensitivity",
              br(),
              uiOutput(ns("sec_stage5_gallery_controls")),
              uiOutput(ns("sec_stage5_gallery"))
            ),
            
            tabPanel(
              title = "Individual Modifier",
              br(),
              uiOutput(ns("sec_stage6_gallery_controls")),
              uiOutput(ns("sec_stage6_gallery"))
            )
          )
        )
      )
    )
  )
}