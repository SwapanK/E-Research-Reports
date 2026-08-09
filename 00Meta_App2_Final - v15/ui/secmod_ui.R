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

# Helper: control section with title and grid
sec_control_section <- function(title, ..., section_class = "sec2-control-section") {
  div(
    class = section_class,
    div(class = "sec2-control-section-title", title),
    div(class = "sec2-control-section-grid", ...)
  )
}

# -----------------------------------------------------------------------------
# Extended override panel for gallery cards (stages 5 & 6, internally s5 & s6)
# -----------------------------------------------------------------------------
sec_plot_card_gallery <- function(ns, key, label, prefix,
                                  default_w = 9, default_h = 5,
                                  default_axis_text = 12, default_axis_title = 14,
                                  default_plot_title = 16, default_strip_text = 12,
                                  default_legend_text = 10,
                                  default_axis_angle = 90, default_legend_key_size = 0.8,
                                  default_panel_spacing = 0.5,
                                  default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                                  default_col_pen = "#F0B323", default_col_cred = "#6FACDE",
                                  default_axis_text_margin_t = 5, default_axis_text_vjust = 1) {
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
      # ---- 1. Plot Layout Properties ----
      # "Statename text size" (facet strip size) only matters for Individual
      # Modifier (s6), which facets by STATECODE. State Sensitivity (s5)
      # plots don't facet, so the control is dropped there.
      sec_control_section(
        "Plot Layout Properties",
        numericInput(ns(paste0(prefix, "_w_", key)), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
        numericInput(ns(paste0(prefix, "_h_", key)), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
        numericInput(ns(paste0(prefix, "_dpi_", key)), "DPI", value = 300, min = 72, max = 600, step = 10, width = "80px"),
        checkboxInput(ns(paste0(prefix, "_bg_", key)), "Transparent background", value = FALSE),
        numericInput(ns(paste0(prefix, "_plot_title_", key)), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
        if (prefix != "s5") {
          numericInput(ns(paste0(prefix, "_strip_text_", key)), "Statename text size", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
        }
      ),
      # ---- 2. Axis Properties ----
      sec_control_section(
        "Axis Properties",
        numericInput(ns(paste0(prefix, "_axis_text_", key)), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_title_", key)), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_angle_", key)), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_text_margin_t_", key)), "X label gap", value = default_axis_text_margin_t, min = 0, max = 50, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_text_vjust_", key)), "X label vjust", value = default_axis_text_vjust, min = 0, max = 1, step = 0.05, width = "80px")
      ),
      # ---- 3. Legend & Panel Properties ----
      # "Panel spacing" only has a visible effect where there are facet panels
      # to space apart (s6). Dropped for s5, which never facets.
      sec_control_section(
        "Legend & Panel Properties",
        checkboxInput(ns(paste0(prefix, "_legend_show_", key)), "Show legend", value = TRUE),
        numericInput(ns(paste0(prefix, "_legend_text_", key)), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_key_size_", key)), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
        if (prefix != "s5") {
          numericInput(ns(paste0(prefix, "_panel_spacing_", key)), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px")
        }
      ),
      # ---- 4. Color Properties ----
      # State Sensitivity (s5) plots are filled by Min/Max, recoloured via
      # Penalty/Credit — SFD/COM colours are never applied there, so they're
      # hidden. Individual Modifier (s6) plots are filled by SFD/COM, so the
      # Penalty/Credit pickers are hidden there instead.
      sec_control_section(
        "Color Properties",
        if (prefix != "s5") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_col_sfd_", key)), "SFD", value = default_col_sfd, showColour = "both", width = "80px")),
        if (prefix != "s5") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_col_com_", key)), "COM", value = default_col_com, showColour = "both", width = "80px")),
        if (prefix != "s6") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_col_pen_", key)), "Penalty", value = default_col_pen, showColour = "both", width = "80px")),
        if (prefix != "s6") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_col_cred_", key)), "Credit", value = default_col_cred, showColour = "both", width = "80px"))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Gallery controls – with grouped layout, toggle, and "Add all to Cart"
# (simplified to match the new control set)
# -----------------------------------------------------------------------------
sec_gallery_controls_ui <- function(ns, prefix, n, default_w, default_h, default_dpi = 300,
                                    default_axis_text = 12, default_axis_title = 14,
                                    default_plot_title = 16, default_strip_text = 12,
                                    default_legend_text = 10,
                                    default_axis_angle = 90, 
                                    default_col_sfd = "#6FACDE", default_col_com = "#F0B323",
                                    default_col_pen = "#F0B323", default_col_cred = "#6FACDE",
                                    default_legend_key_size = 0.8,
                                    default_panel_spacing = 0.5,
                                    default_bg = FALSE,
                                    default_axis_text_margin_t = 5, default_axis_text_vjust = 1) {
  div(
    class = "sec-gallery-bar sec-gallery-collapsed",  # default collapsed
    div(
      class = "sec-gallery-bar-head",
      div(class = "sec-gallery-bar-title", icon("sliders-h"), "Gallery defaults"),
      div(
        style = "display:flex; align-items:center; gap:8px;",
        span(class = "sec-gallery-bar-count", paste(n, "plots")),
        tags$button(
          class = "sec-gallery-toggle",
          onclick = "toggleGallery(this)",
          icon("chevron-down")
        )
      )
    ),
    # ---- Body (collapsible) ----
    div(
      class = "sec-gallery-body",
      sec_hint("Changes take effect after clicking ‘Apply to all’ below."),
      
      # ---- Grouped controls ----
      div(
        class = "sec-gallery-controls",
        
        # 1. Plot Layout Properties
        sec_control_section(
          "Plot Layout Properties",
          numericInput(ns(paste0(prefix, "_default_w")), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_h")), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_dpi")), "DPI", value = default_dpi, min = 72, max = 600, step = 10, width = "80px"),
          checkboxInput(ns(paste0(prefix, "_default_bg")), "Transparent background", value = isTRUE(default_bg)),
          numericInput(ns(paste0(prefix, "_default_plot_title")), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
          if (prefix != "s5") {
            numericInput(ns(paste0(prefix, "_default_strip_text")), "Statename text size", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
          }
        ),
        
        # 2. Axis Properties
        sec_control_section(
          "Axis Properties",
          numericInput(ns(paste0(prefix, "_default_axis_text")), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_title")), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_angle")), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_text_margin_t")), "X label gap", value = default_axis_text_margin_t, min = 0, max = 50, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_text_vjust")), "X label vjust", value = default_axis_text_vjust, min = 0, max = 1, step = 0.05, width = "80px")
        ),
        
        # 3. Legend & Panel Properties
        sec_control_section(
          "Legend & Panel Properties",
          checkboxInput(ns(paste0(prefix, "_legend_show")), "Show legend", value = TRUE),
          numericInput(ns(paste0(prefix, "_default_legend_text")), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_key_size")), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
          if (prefix != "s5") {
            numericInput(ns(paste0(prefix, "_default_panel_spacing")), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px")
          }
        ),
        
        # 4. Color Properties
        sec_control_section(
          "Color Properties",
          if (prefix != "s5") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_default_col_sfd")), "SFD", value = default_col_sfd, showColour = "both", width = "80px")),
          if (prefix != "s5") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_default_col_com")), "COM", value = default_col_com, showColour = "both", width = "80px")),
          if (prefix != "s6") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_default_col_pen")), "Penalty", value = default_col_pen, showColour = "both", width = "80px")),
          if (prefix != "s6") div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_default_col_cred")), "Credit", value = default_col_cred, showColour = "both", width = "80px"))
        )
      ),
      
      div(
        class = "sec-gallery-bar-foot",
        div(
          style = "display:flex; gap:10px; flex-wrap:nowrap; align-items:center; justify-content:flex-end;",
          actionButton(ns(paste0(prefix, "_add_all_cart")), paste0("Add all ", n, " plots to Cart"),
                       icon = icon("cart-plus"), class = "btn-download-csv",
                       style = "background: #EAF4FC !important; color: #0075BC !important; white-space:nowrap;"),
          actionButton(ns(paste0(prefix, "_apply_all")), paste0("Apply to all ", n, " plots"),
                       icon = icon("wand-magic-sparkles"), class = "sec2-btn",
                       style = "white-space:nowrap;")
        )
      )
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
      
      # ---- Inline CSS for progress stepper and gallery toggle ----
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
      
      # ---- JavaScript for scrolling and gallery toggle ----
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

        function toggleGallery(el) {
          var bar = el.closest('.sec-gallery-bar');
          if (bar) {
            bar.classList.toggle('sec-gallery-collapsed');
            var icon = el.querySelector('i');
            if (icon) {
              icon.classList.toggle('fa-chevron-down');
              icon.classList.toggle('fa-chevron-up');
            }
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
        // NOTE: this intentionally does NOT gate any of its setup behind
        // 'shiny:connected'. That event only fires once, early in the app's
        // life; panels built via renderUI (like the colour swatch cards) can
        // be inserted well before or after it fires, so listeners registered
        // only inside a 'shiny:connected' handler can silently miss them.
        function secmodContrastText(hex) {
          if (!hex) return null;
          hex = hex.replace('#', '').trim();
          if (hex.length === 3) hex = hex.split('').map(function(c) { return c + c; }).join('');
          if (hex.length !== 6 || /[^0-9a-fA-F]/.test(hex)) return null;
          var r = parseInt(hex.substr(0, 2), 16),
              g = parseInt(hex.substr(2, 2), 16),
              b = parseInt(hex.substr(4, 2), 16);
          var luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
          return luminance > 0.6 ? '#1a1a1a' : '#ffffff';
        }
        function secmodSyncSwatch(el) {
          var wrap = el.closest('.sec2-swatch-field');
          if (!wrap) return;
          var val = el.value || el.getAttribute('data-init-value') || el.getAttribute('value');
          if (!val) return;
          wrap.style.setProperty('--swatch-color', val);
          var textColor = secmodContrastText(val);
          if (textColor) wrap.style.setProperty('--swatch-text', textColor);
        }
        function secmodSyncAllSwatches() {
          document.querySelectorAll('.sec2-swatch-field input.shiny-colour-input')
            .forEach(secmodSyncSwatch);
        }

        // Delegated listener: works no matter when the input is inserted.
        $(document).on('input change', '.sec2-swatch-field input.shiny-colour-input', function() {
          secmodSyncSwatch(this);
        });

        // Run right away for anything already in the DOM, plus a couple of
        // follow-up passes shortly after in case the colourpicker binding
        // hadn't finished initializing yet.
        secmodSyncAllSwatches();
        setTimeout(secmodSyncAllSwatches, 300);
        setTimeout(secmodSyncAllSwatches, 1000);
        $(document).on('shiny:connected', secmodSyncAllSwatches);

        // ---- Dynamically-rendered swatches (gallery controls/cards via renderUI) ----
        // Registered immediately so it catches panels rendered either before
        // or long after 'shiny:connected'.
        var secmodSwatchObserver = new MutationObserver(function() {
          secmodSyncAllSwatches();
        });
        if (document.body) {
          secmodSwatchObserver.observe(document.body, { childList: true, subtree: true });
        }

        // Final safety net: brief low-frequency poll that self-stops, in case
        // a swatch appears through a path the observer doesn't catch.
        var secmodSwatchPollCount = 0;
        var secmodSwatchPoll = setInterval(function() {
          secmodSyncAllSwatches();
          secmodSwatchPollCount++;
          if (secmodSwatchPollCount > 40) clearInterval(secmodSwatchPoll);
        }, 500);
      ", ns("sec_cart_click"))))
    ),
    
    div(
      class = "app-shell",
      
      # ===== HEADER =====
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Secondary Modifier")
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
            id = "sec-settings-card",
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
            textInput(ns("sec_model_version"), "Model version", value = "HD", width = "100%"),
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
            id = "sec-inputs-card",
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("2. Inputs"),
              uiOutput(ns("sec_stage1_status"), inline = TRUE)
            ),
            sec_hint("Upload State AAL, Country AAL and the SecMod mapping file (.csv). All three are required."),
            
            fileInput(ns("sec_file_state"), "Load State AAL (.csv)", accept = ".csv", width = "100%"),
            fileInput(ns("sec_file_usa"), "Load Country AAL (.csv)", accept = ".csv", width = "100%"),
            fileInput(ns("sec_file_mapping"), "Load SecMod File (.csv)", accept = ".csv", width = "100%"),
            
            # ---- Sample download buttons and Load Demo Data ----
            div(
              style = "display: flex; flex-direction: column; gap: 6px; width: 100%;",
              downloadButton(ns("sec_dl_sample_state"), "Download sample State AAL (.csv)", class = "btn-default"),
              downloadButton(ns("sec_dl_sample_country"), "Download sample Country AAL (.csv)", class = "btn-default"),
              downloadButton(ns("sec_dl_sample_mapping"), "Download sample SecMod File (.csv)", class = "btn-default"),
              actionButton(ns("sec_load_demo"), "Load Demo Data", icon = icon("play"), class = "primary-btn")
            ),
            
            tags$hr(class = "soft"),
            
            # ---- Colour pickers REMOVED (now per-plot) ----
            
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
          
          # ---- Section 3: Credit / Penalty ----
          div(
            id = "sec-credit-card",
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("3. Credit / Penalty"),
              uiOutput(ns("sec_stage4_status"), inline = TRUE)
            ),
            sec_hint("Shows which modifiers move loss cost by more than 10% — split into credits (below 1.0) and penalties (above 1.0)."),
            actionButton(ns("sec_credit"), "Generate Credit / Penalty plot", icon = icon("play"), class = "primary-btn")
          ),
          
          # ---- Section 4: State Sensitivity ----
          div(
            id = "sec-state-card",
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("4. State Sensitivity"),
              uiOutput(ns("sec_stage5_status"), inline = TRUE)
            ),
            sec_hint("Builds one chart per state comparing SFD vs COM — this renders every state individually, so it can take a little while."),
            actionButton(ns("sec_state_plots"), "Generate all State plots", icon = icon("play"), class = "primary-btn")
          ),
          
          # ---- Section 5: Individual Modifier ----
          div(
            id = "sec-modifier-card",
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
            id = "sec-report-card",
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
            p("This app compares various ", strong("secondary modifiers"), " in vendor models. Secondary modifiers are presented in a ", strong("credit-penalty"), " format. ", em("Credit"), " implies that a particular modifier contributes to a ", em("reduction"), " in average annual loss (AAL), relative to the ", em("unknown"), " case, expressed in percentage terms, whereas ", em("penalty"), " implies that a modifier contributes to an ", em("increase"), " in AAL. The following three exhibits are presented:"),
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
          
          # ---- Output tabs (new order) ----
          tabsetPanel(
            id = ns("sec_output_tabs"),
            
            # New review tabs (1-3)
            tabPanel(
              title = "State AAL",
              br(),
              div(
                style = "margin-bottom: 12px;",
                uiOutput(ns("sec_state_aal_summary"))
              ),
              DTOutput(ns("sec_state_aal_table"))
            ),
            tabPanel(
              title = "Country AAL",
              br(),
              div(
                style = "margin-bottom: 12px;",
                uiOutput(ns("sec_country_aal_summary"))
              ),
              DTOutput(ns("sec_country_aal_table"))
            ),
            tabPanel(
              title = "Mapping",
              br(),
              div(
                style = "margin-bottom: 12px;",
                uiOutput(ns("sec_mod_file_summary"))
              ),
              DTOutput(ns("sec_mod_file_table"))
            ),
            
            # Existing tabs (4-7)
            tabPanel(
              title = "Min / max Table",
              br(),
              div(class = "sec-table-wrap", DTOutput(ns("sec_minmax_tbl")))
            ),
            tabPanel(
              title = "Credit / Penalty",
              br(),
              uiOutput(ns("sec_credit_card"))
            ),
            tabPanel(
              title = "State Sensitivity",
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




