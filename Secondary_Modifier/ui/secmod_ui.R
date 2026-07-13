
library(shiny)
library(shinyjs)
library(colourpicker)

# =============================================================================
# SECONDARY MODIFIER UI
# =============================================================================

sec_stage_header <- function(id, number, title, subtitle) {
  div(
    id    = paste0(id, "_header"),
    class = "glass-card sec-stage-header",
    style = "cursor:pointer; display:flex; align-items:center; justify-content:space-between; margin-bottom:0;",
    div(
      style = "display:flex; align-items:center; gap:12px;",
      span(
        id    = paste0(id, "_number"),
        class = "sec-stage-number",
        style = "width:28px; height:28px; border-radius:50%;
                  display:flex; align-items:center; justify-content:center; font-weight:700; flex-shrink:0;",
        number
      ),
      div(
        h4(style = "margin:0;", title),
        tags$small(style = "color:#718096;", subtitle)
      )
    ),
    uiOutput(paste0(id, "_status"), inline = TRUE)
  )
}

# -----------------------------------------------------------------------------
# Extended override panel for gallery cards (stage 5 & 6)
# Now with initial values set to the defaults passed from the gallery controls
# -----------------------------------------------------------------------------
sec_plot_card_gallery <- function(key, label, prefix, 
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
  ov_id <- paste0(prefix, "_override_", key)
  
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
        # REPLACED: direct downloadButton
        downloadButton(
          outputId = paste0(prefix, "_dl_", key),
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
    
    # Plot frame with dynamic height
    div(
      class = "sec-plot-frame",
      uiOutput(paste0(prefix, "_plot_frame_", key))
    ),
    
    # Collapsible control panel – all inputs pre‑filled with defaults
    div(
      id = ov_id, class = "sec-override-panel",
      numericInput(paste0(prefix, "_w_", key), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_h_", key), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_axis_text_", key), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_axis_title_", key), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_plot_title_", key), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_strip_text_", key), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_legend_text_", key), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_legend_title_", key), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_axis_angle_", key), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_legend_key_size_", key), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
      numericInput(paste0(prefix, "_plot_title_hjust_", key), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
      numericInput(paste0(prefix, "_panel_spacing_", key), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_plot_margin_t_", key), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_plot_margin_r_", key), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_plot_margin_b_", key), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_plot_margin_l_", key), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
      selectInput(paste0(prefix, "_legend_pos_", key), "Legend", 
                  choices = c("top", "bottom", "left", "right", "none"), 
                  selected = "top", width = "80px"),
      checkboxInput(paste0(prefix, "_legend_show_", key), "Show legend", value = TRUE),
      colourInput(paste0(prefix, "_axis_line_col_", key), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_panel_fill_", key), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_grid_col_", key), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_panel_border_col_", key), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
      numericInput(paste0(prefix, "_panel_border_lwd_", key), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px"),
      colourInput(paste0(prefix, "_col_sfd_", key), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_col_com_", key), "COM", value = default_col_com, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_col_pen_", key), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_col_cred_", key), "Credit", value = default_col_cred, showColour = "text", width = "80px")
    )
  )
}

# -----------------------------------------------------------------------------
# Gallery controls – with all default values (unchanged)
# -----------------------------------------------------------------------------
sec_gallery_controls_ui <- function(prefix, n, default_w, default_h, default_dpi,
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
    div(
      class = "sec-gallery-bar-grid",
      numericInput(paste0(prefix, "_default_w"), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_default_h"), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_default_dpi"), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
      numericInput(paste0(prefix, "_default_axis_text"), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_axis_title"), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_plot_title"), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_strip_text"), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_legend_text"), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_legend_title"), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
      numericInput(paste0(prefix, "_default_axis_angle"), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_default_legend_key_size"), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px"),
      numericInput(paste0(prefix, "_default_title_hjust"), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
      numericInput(paste0(prefix, "_default_panel_spacing"), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
      numericInput(paste0(prefix, "_default_margin_t"), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_default_margin_r"), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_default_margin_b"), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
      numericInput(paste0(prefix, "_default_margin_l"), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
      selectInput(paste0(prefix, "_default_legend_pos"), "Legend", choices = c("top","bottom","left","right","none"), selected = default_legend_pos, width = "80px"),
      selectInput(paste0(prefix, "_default_bg"), "Background", choices = c("White" = "white", "Transparent" = "transparent"), selected = default_bg, width = "100px"),
      colourInput(paste0(prefix, "_default_axis_line_col"), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_panel_fill"), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_grid_col"), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_border_col"), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
      numericInput(paste0(prefix, "_default_border_lwd"), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px"),
      colourInput(paste0(prefix, "_default_col_sfd"), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_col_com"), "COM", value = default_col_com, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_col_pen"), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
      colourInput(paste0(prefix, "_default_col_cred"), "Credit", value = default_col_cred, showColour = "text", width = "80px")
    ),
    div(
      class = "sec-gallery-bar-foot",
      actionButton(paste0(prefix, "_apply_all"), paste0("Apply to all ", n, " plots"),
                   icon = icon("wand-magic-sparkles"), class = "btn-glass")
    )
  )
}

secmod_ui <- function() {
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        function secmodCartClick(key) {
          Shiny.setInputValue('sec_cart_click', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
        // REMOVED secmodDownloadClick – no longer needed
      ")),
      tags$style(HTML("
        /* ------------------------------------------------------------------
           Plot control panel (Adjust size, colours, text & legend)
           Was a bare flex-wrap list of unstyled Shiny inputs on white --
           now a soft violet/indigo card with each control in its own
           chip, laid out on a grid so columns line up cleanly instead of
           the previous ragged flex-wrap rows.
        ------------------------------------------------------------------ */
        /* ------------------------------------------------------------------
           Gallery defaults bar -- was a plain grey flex row; now matches
           the same violet/indigo card language as the per-plot override
           panel, but always visible (not collapsible) with its own
           header + right-aligned Apply action.
        ------------------------------------------------------------------ */
        .sec-gallery-bar {
          margin-bottom: 16px;
          padding: 16px 18px 18px;
          border-radius: 14px;
          background: linear-gradient(135deg, #F5F3FF 0%, #EEF2FF 55%, #FDF2F8 100%);
          border: 1px solid rgba(124, 58, 237, 0.14);
          box-shadow: 0 4px 16px rgba(102, 126, 234, 0.08);
        }
        .sec-gallery-bar-head {
          display: flex; align-items: center; justify-content: space-between;
          margin-bottom: 12px;
        }
        .sec-gallery-bar-title {
          display: flex; align-items: center; gap: 8px;
          font-size: 13px; font-weight: 700; text-transform: uppercase;
          letter-spacing: .045em; color: #6D28D9;
        }
        .sec-gallery-bar-title svg { color: #7C3AED; }
        .sec-gallery-bar-count {
          font-size: 11px; font-weight: 700; color: #7C3AED;
          background: rgba(124,58,237,0.10); border-radius: 20px;
          padding: 3px 10px;
        }
        .sec-gallery-bar-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(96px, 1fr));
          gap: 14px 12px;
          align-items: end;
        }
        .sec-gallery-bar-grid .shiny-input-container {
          background: rgba(255,255,255,0.78);
          border: 1px solid rgba(124,58,237,0.12);
          border-radius: 10px;
          padding: 8px 10px 7px;
          margin: 0;
          width: auto !important;
          transition: box-shadow .15s ease, border-color .15s ease;
        }
        .sec-gallery-bar-grid .shiny-input-container:focus-within {
          border-color: #7C3AED;
          box-shadow: 0 0 0 3px rgba(124,58,237,0.14);
        }
        .sec-gallery-bar-grid .control-label {
          display:block;
          font-size: 10px;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: .045em;
          color: #6D28D9;
          margin-bottom: 5px;
          white-space: nowrap;
        }
        .sec-gallery-bar-grid input[type='number'],
        .sec-gallery-bar-grid input[type='text'] {
          width: 100% !important;
          border: 1px solid #DDD6FE !important;
          border-radius: 6px !important;
          padding: 4px 7px !important;
          font-size: 12.5px !important;
          background: #fff !important;
          box-shadow: none !important;
          height: 30px !important;
        }
        .sec-gallery-bar-grid input:focus {
          border-color: #7C3AED !important;
          outline: none !important;
          box-shadow: none !important;
        }
        .sec-gallery-bar-grid .selectize-control .selectize-input {
          border: 1px solid #DDD6FE !important;
          border-radius: 6px !important;
          min-height: 30px !important;
          padding: 4px 8px !important;
          font-size: 12.5px !important;
          box-shadow: none !important;
        }
        .sec-gallery-bar-grid .selectize-input.focus {
          border-color: #7C3AED !important;
          box-shadow: none !important;
        }
        .sec-gallery-bar-foot {
          display: flex; justify-content: flex-end;
          margin-top: 14px;
        }
        .sec-override-panel {
          display:none;
          margin-top:12px;
          padding:18px 20px;
          border-radius:14px;
          background: linear-gradient(135deg, #F5F3FF 0%, #EEF2FF 55%, #FDF2F8 100%);
          border: 1px solid rgba(124, 58, 237, 0.14);
          box-shadow: 0 4px 16px rgba(102, 126, 234, 0.08);
        }
        .sec-override-panel.sec-open {
          display:grid;
          grid-template-columns: repeat(auto-fill, minmax(96px, 1fr));
          gap: 14px 12px;
          align-items: end;
        }
        .sec-override-panel .shiny-input-container {
          background: rgba(255,255,255,0.78);
          border: 1px solid rgba(124,58,237,0.12);
          border-radius: 10px;
          padding: 8px 10px 7px;
          margin: 0;
          width: auto !important;
          transition: box-shadow .15s ease, border-color .15s ease;
        }
        .sec-override-panel .shiny-input-container:focus-within {
          border-color: #7C3AED;
          box-shadow: 0 0 0 3px rgba(124,58,237,0.14);
        }
        .sec-override-panel .control-label {
          display:block;
          font-size: 10px;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: .045em;
          color: #6D28D9;
          margin-bottom: 5px;
          white-space: nowrap;
        }
        .sec-override-panel input[type='number'],
        .sec-override-panel input[type='text'] {
          width: 100% !important;
          border: 1px solid #DDD6FE !important;
          border-radius: 6px !important;
          padding: 4px 7px !important;
          font-size: 12.5px !important;
          background: #fff !important;
          box-shadow: none !important;
          height: 30px !important;
        }
        .sec-override-panel input:focus {
          border-color: #7C3AED !important;
          outline: none !important;
          box-shadow: none !important;
        }
        .sec-override-panel .selectize-control .selectize-input {
          border: 1px solid #DDD6FE !important;
          border-radius: 6px !important;
          min-height: 30px !important;
          padding: 4px 8px !important;
          font-size: 12.5px !important;
          box-shadow: none !important;
        }
        .sec-override-panel .selectize-input.focus {
          border-color: #7C3AED !important;
          box-shadow: none !important;
        }
        .sec-override-panel .checkbox {
          align-self: center;
          background: transparent;
          border: none;
          box-shadow: none;
          padding: 0;
          margin: 0;
        }
        /* Bootstrap 3 normally makes the checkbox <input> position:absolute
           with a negative margin-left, relying on the <label>'s
           padding-left:20px to reserve space for it. That fights with the
           flex layout below and is what was causing the box and the
           Show legend text to overlap -- reset both back to normal
           in-flow positioning so flex can lay them out cleanly. */
        .sec-override-panel .checkbox label {
          position: static;
          padding-left: 0 !important;
          margin: 0;
          font-size: 12.5px;
          font-weight: 600;
          color: #4C1D95;
          display: flex;
          align-items: center;
          gap: 8px;
          cursor: pointer;
        }
        .sec-override-panel .checkbox input[type='checkbox'] {
          position: static !important;
          float: none !important;
          margin: 0 !important;
          width: 18px;
          height: 18px;
          flex-shrink: 0;
          accent-color: #7C3AED;
          border: 1.5px solid #A78BFA;
          border-radius: 4px;
          cursor: pointer;
        }
        .cart-item-header { display:flex; align-items:center; justify-content:space-between; }
        .sec-shell { display: flex; align-items: flex-start; gap: 24px; }
        .sec-left-rail { position: sticky; top: 14px; width: 360px; flex-shrink: 0; align-self: flex-start; }
        .sec-right-pane { flex: 1; min-width: 0; }
        .sec-stage-block { margin-bottom: 20px; border-radius: 12px; overflow: hidden; border: 1px solid #e7e7f2; box-shadow: 0 1px 4px rgba(15, 23, 42, 0.05); background: #ffffff; }
        .sec-stage-block .glass-card { box-shadow: none; border: none; border-radius: 0; margin-bottom: 0; }
        .sec-stage-block .glass-card + .glass-card { border-top: 1px solid #eceef5; }
        .sec-stage-block .glass-card:first-child { padding: 14px 16px; }
        .sec-stage-block .glass-card:not(:first-child) { padding: 16px; }
        .sec-output-block { margin-bottom: 20px; border-radius: 12px; border: 1px solid #e7e7f2; box-shadow: 0 1px 4px rgba(15, 23, 42, 0.05); background: #ffffff; padding: 16px 18px; }
        .sec-output-block h4 { margin-top: 0; }
        .sec-progress-bar { margin-bottom: 18px; }
        .sec-progress-bar .glass-card { padding: 14px 18px; border-radius: 12px; box-shadow: 0 1px 4px rgba(15, 23, 42, 0.05); }
        .sec-stage-header:hover { background: rgba(15, 23, 42, 0.02); }
        @media (max-width: 900px) { .sec-shell { flex-direction: column; } .sec-left-rail { position: relative; top: 0; width: 100%; } }
      "))
    ),
    
    # HEADER
    div(
      class = "page-header",
      h1(class = "gradient-text", "Secondary Modifier"),
      p("Work through the real secondary-modifier pipeline stage by stage: load data, build tables, summarize, and generate every plot with its own size and cart controls.")
    ),
    
    # PIPELINE PROGRESS – brown themed
    div(class = "sec-progress-bar", div(class = "glass-card", uiOutput("sec_stepper"))),
    
    div(
      class = "sec-shell",
      
      # LEFT RAIL
      div(
        class = "sec-left-rail",
        # Stage 1 – with colourInput
        div(class = "sec-stage-block",
            sec_stage_header("stage1", 1, "Load data", "aal_State, aal_USA, mapping"),
            hidden(div(
              id = "stage1_body", class = "glass-card",
              fileInput("sec_file_state", "aal_State (.rds or .csv)", accept = c(".rds", ".csv")),
              fileInput("sec_file_usa",   "aal_USA (.rds or .csv)",   accept = c(".rds", ".csv")),
              checkboxInput("sec_use_default_mapping", "Use built-in modifier mapping", value = TRUE),
              conditionalPanel("!input.sec_use_default_mapping",
                               fileInput("sec_file_mapping", "SecMod_name mapping (.csv)", accept = ".csv")),
              tags$hr(),
              fluidRow(
                column(6, colourInput("sec_color_sfd", "SFD color", value = "#6FACDE", showColour = "text")),
                column(6, colourInput("sec_color_com", "COM color", value = "#F0B323", showColour = "text"))
              ),
              fluidRow(
                column(6, colourInput("sec_color_max", "Penalty color", value = "#F0B323", showColour = "text")),
                column(6, colourInput("sec_color_min", "Credit color", value = "#6FACDE", showColour = "text"))
              ),
              actionButton("sec_load", "Load data", icon = icon("upload"), class = "btn-glass", width = "100%")
            ))
        ),
        # Stage 2
        div(class = "sec-stage-block",
            sec_stage_header("stage2", 2, "Build tables", "finaltable() / finaltable_allUSA(), or upload CSVs"),
            hidden(div(
              id = "stage2_body", class = "glass-card",
              actionButton("sec_build", "Build tables from loaded data", icon = icon("cogs"), class = "btn-glass", width = "100%"),
              tags$hr(),
              downloadButton("sec_dl_final", "aal_final.csv"),
              downloadButton("sec_dl_final_usa", "aal_final_USA.csv"),
              tags$hr(),
              p(class = "commentary-text", style = "font-size:12px;",
                "Or skip stage 1+2 by uploading previously downloaded CSVs:"),
              fileInput("sec_upload_final", "Upload aal_final.csv", accept = ".csv"),
              fileInput("sec_upload_final_usa", "Upload aal_final_USA.csv", accept = ".csv")
            ))
        ),
        # Stage 3
        div(class = "sec-stage-block",
            sec_stage_header("stage3", 3, "Min / max summary", "STATEminmax(), Countryminmax(), CountryminmaxTable()"),
            hidden(div(
              id = "stage3_body", class = "glass-card",
              actionButton("sec_minmax", "Compute min / max", icon = icon("calculator"), class = "btn-glass", width = "100%")
            ))
        ),
        # Stage 4 – button only, card is rendered in right pane
        div(class = "sec-stage-block",
            sec_stage_header("stage4", 4, "Credit / penalty", "Credit_Penalty() — modifiers with >10% sensitivity"),
            hidden(div(
              id = "stage4_body", class = "glass-card",
              actionButton("sec_credit", "Generate credit / penalty chart", icon = icon("play"), class = "btn-glass", width = "100%")
            ))
        ),
        # Stage 5
        div(class = "sec-stage-block",
            sec_stage_header("stage5", 5, "State sensitivity", "STATE_plot() for every state and LOB, one by one"),
            hidden(div(
              id = "stage5_body", class = "glass-card",
              actionButton("sec_state_plots", "Generate all state plots", icon = icon("play"), class = "btn-glass", width = "100%"),
              tags$hr(),
              uiOutput("sec_stage5_gallery_controls")
            ))
        ),
        # Stage 6
        div(class = "sec-stage-block",
            sec_stage_header("stage6", 6, "Modifier detail", "indmod() for every modifier, one by one"),
            hidden(div(
              id = "stage6_body", class = "glass-card",
              actionButton("sec_modifier_plots", "Generate all modifier plots", icon = icon("play"), class = "btn-glass", width = "100%"),
              tags$hr(),
              uiOutput("sec_stage6_gallery_controls")
            ))
        )
      ),
      
      # RIGHT PANE
      div(
        class = "sec-right-pane",
        uiOutput("sec_stage2_summary"),
        div(class = "sec-output-block",
            h4(style = "margin-top:0;", "Min / max summary"),
            div(class = "sec-table-wrap", tableOutput("sec_minmax_tbl"))
        ),
        div(class = "sec-output-block",
            h4(style = "margin-top:0;", "Credit / penalty"),
            uiOutput("sec_credit_card")   # only here
        ),
        div(class = "sec-output-block",
            h4(style = "margin-top:0;", "State sensitivity gallery"),
            uiOutput("sec_stage5_gallery")
        ),
        div(class = "sec-output-block",
            h4(style = "margin-top:0;", "Modifier detail gallery"),
            uiOutput("sec_stage6_gallery")
        )
      )
    )
    
    # REMOVED hidden downloadButton("sec_gallery_download")
  )
}