
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
# Extended override panel for gallery cards (stages 5 & 6)
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
      ", ns("sec_cart_click"))))
    ),
    
    div(
      class = "single-scroll-panel",
      
      # HEADER
      div(
        class = "app-header-plain",
        h1("Secondary Modifier"),
        p("Work through the secondary modifier pipeline stage by stage: load data, build tables, summarize, and generate every plot with its own size and cart controls.")
      ),
      
      # PROGRESS STRIP (same as before)
      uiOutput(ns("sec_stepper")),
      
      br(),
      
      # TABBED WIZARD
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
                   downloadButton(ns("sec_dl_final"), "Download aal_final.csv"),
                   downloadButton(ns("sec_dl_final_usa"), "Download aal_final_USA.csv")
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
      
      # ----- STAGE 4 — CREDIT / PENALTY --------------------------------------
      tabPanel(
        title = sec_tab_title(4, "Credit / penalty"), value = "stage4",
        div(
          class = "glass-card sec-tab-panel",
          div(
            style = "display:flex; align-items:center; justify-content:space-between;",
            tags$small(style = "color:#718096;", "Credit_Penalty() — modifiers with >10% sensitivity"),
            uiOutput(ns("sec_stage4_status"), inline = TRUE)
          ),
          tags$hr(),
          actionButton(ns("sec_credit"), "Generate credit / penalty chart", icon = icon("play"), class = "btn-glass"),
          tags$hr(),
          uiOutput(ns("sec_credit_card"))
        )
      ),
      
      # ----- STAGE 5 — STATE SENSITIVITY -------------------------------------
      tabPanel(
        title = sec_tab_title(5, "State sensitivity"), value = "stage5",
        div(
          class = "glass-card sec-tab-panel",
          div(
            style = "display:flex; align-items:center; justify-content:space-between;",
            tags$small(style = "color:#718096;", "STATE_plot() for every state and LOB, rendered one by one"),
            uiOutput(ns("sec_stage5_status"), inline = TRUE)
          ),
          tags$hr(),
          actionButton(ns("sec_state_plots"), "Generate all state plots", icon = icon("play"), class = "btn-glass"),
          tags$hr(),
          uiOutput(ns("sec_stage5_gallery_controls")),
          uiOutput(ns("sec_stage5_gallery"))
        )
      ),
      
      # ----- STAGE 6 — MODIFIER DETAIL ---------------------------------------
      tabPanel(
        title = sec_tab_title(6, "Modifier detail"), value = "stage6",
        div(
          class = "glass-card sec-tab-panel",
          div(
            style = "display:flex; align-items:center; justify-content:space-between;",
            tags$small(style = "color:#718096;", "indmod() for every modifier, rendered one by one"),
            uiOutput(ns("sec_stage6_status"), inline = TRUE)
          ),
          tags$hr(),
          actionButton(ns("sec_modifier_plots"), "Generate all modifier plots", icon = icon("play"), class = "btn-glass"),
          tags$hr(),
          uiOutput(ns("sec_stage6_gallery_controls")),
          uiOutput(ns("sec_stage6_gallery"))
        )
      )
      ),
      
      br()
    )
  )
}


