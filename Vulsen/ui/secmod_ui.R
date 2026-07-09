library(shiny)
library(shinyjs)

# =============================================================================
# SECONDARY MODIFIER UI  (module/function_Secmod.R driven, real-data version)
# =============================================================================
# Six collapsible stages mirror function_Secmod.R's pipeline exactly:
#   1. Load data          -> raw aal_State / aal_USA / SecMod_name
#   2. Build tables        -> finaltable() / finaltable_allUSA()
#   3. Min/max summary     -> STATEminmax() / Countryminmax() / CountryminmaxTable()
#   4. Credit / penalty    -> Credit_Penalty()
#   5. State sensitivity   -> STATE_plot() for every STATECODE x LOB, one by one
#   6. Modifier detail     -> indmod() for every modifier, one by one
#
# Each stage keeps its own upload/download controls. Stage 4 is a single named
# plot with a full control strip. Stages 5/6 are "render all, one by one"
# lists where each plot card carries its own size override on top of a
# gallery-wide default.
# =============================================================================

# -- small helper: one stage header (click to expand/collapse) --------------
sec_stage_header <- function(id, number, title, subtitle) {
  div(
    id    = paste0(id, "_header"),
    class = "glass-card",
    style = "cursor:pointer; display:flex; align-items:center; justify-content:space-between; margin-bottom:0;",

    div(
      style = "display:flex; align-items:center; gap:12px;",
      span(
        class = "sec-stage-number",
        style = "width:28px; height:28px; border-radius:50%; background:#eef2ff; color:#4338ca;
                  display:flex; align-items:center; justify-content:center; font-weight:600; flex-shrink:0;",
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

secmod_ui <- function() {

  fluidPage(

    useShinyjs(),

    tags$head(
      tags$script(HTML("
        function secmodCartClick(key) {
          Shiny.setInputValue('sec_cart_click', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
        function secmodDownloadClick(key) {
          Shiny.setInputValue('sec_download_click', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
      ")),
      tags$style(HTML("
        .sec-override-panel { display:none; gap:12px; margin-top:8px; padding-top:8px; border-top:0.5px solid #e2e8f0; }
        .sec-override-panel.sec-open { display:flex; }
        .cart-item-header { display:flex; align-items:center; justify-content:space-between; }

        /* plot cards resize smoothly instead of jumping when width/height change */
        .sec-plot-card { transition: margin-bottom 0.2s ease; }
        .sec-plot-frame { transition: height 0.25s ease; overflow: hidden; border-radius: 6px; }
        .sec-plot-frame .shiny-plot-output { transition: height 0.25s ease; }

        /* stepper */
        .sec-stepper-dot { transition: all 0.25s ease; }
        .sec-stepper-current { box-shadow: 0 0 0 4px rgba(67, 56, 202, 0.15); }
      "))
    ),
    


    # =========================================================================
    # HEADER
    # =========================================================================

    div(
      class = "page-header",
      h1(class = "gradient-text", "Secondary Modifier"),
      p("Work through the real secondary-modifier pipeline stage by stage: load data, build tables, summarize, and generate every plot with its own size and cart controls.")
    ),

    # =========================================================================
    # STAGE PROGRESS STRIP
    # =========================================================================

    uiOutput("sec_stepper"),

    br(),

    # =========================================================================
    # STAGE 1 — LOAD DATA
    # =========================================================================

    sec_stage_header("stage1", 1, "Load data", "aal_State, aal_USA, modifier mapping, type colors"),

    hidden(div(
      id = "stage1_body",
      class = "glass-card",

      fluidRow(
        column(4,
          fileInput("sec_file_state", "aal_State (.rds or .csv)", accept = c(".rds", ".csv")),
        ),
        column(4,
          fileInput("sec_file_usa", "aal_USA (.rds or .csv)", accept = c(".rds", ".csv")),
        ),
        column(4,
          checkboxInput("sec_use_default_mapping", "Use built-in modifier mapping", value = TRUE),
          conditionalPanel(
            "!input.sec_use_default_mapping",
            fileInput("sec_file_mapping", "SecMod_name mapping (.csv)", accept = ".csv")
          )
        )
      ),

      tags$hr(),

      fluidRow(
        column(3,
          textInput("sec_color_sfd", "SFD color (hex)", value = "#6FACDE")
        ),
        column(3,
          textInput("sec_color_com", "COM color (hex)", value = "#F0B323")
        ),
        column(3,
          textInput("sec_color_max", "Penalty color (hex)", value = "#F0B323")
        ),
        column(3,
          textInput("sec_color_min", "Credit color (hex)", value = "#6FACDE")
        )
      ),

      div(
        actionButton("sec_load", "Load data", icon = icon("upload"), class = "btn-glass")
      )
    )),

    br(),

    # =========================================================================
    # STAGE 2 — BUILD TABLES
    # =========================================================================

    sec_stage_header("stage2", 2, "Build tables", "finaltable() / finaltable_allUSA(), or upload pre-built CSVs"),

    hidden(div(
      id = "stage2_body",
      class = "glass-card",

      p(class = "commentary-text", "Either build from the stage 1 data, or skip straight here by uploading previously downloaded aal_final / aal_final_USA CSVs."),

      fluidRow(
        column(6,
          actionButton("sec_build", "Build tables from loaded data", icon = icon("cogs"), class = "btn-glass")
        ),
        column(6,
          downloadButton("sec_dl_final", "Download aal_final.csv"),
          downloadButton("sec_dl_final_usa", "Download aal_final_USA.csv")
        )
      ),

      tags$hr(),

      fluidRow(
        column(6,
          fileInput("sec_upload_final", "Upload aal_final.csv (skip stage 1+2)", accept = ".csv")
        ),
        column(6,
          fileInput("sec_upload_final_usa", "Upload aal_final_USA.csv (skip stage 1+2)", accept = ".csv")
        )
      ),

      uiOutput("sec_stage2_summary")
    )),

    br(),

    # =========================================================================
    # STAGE 3 — MIN/MAX SUMMARY
    # =========================================================================

    sec_stage_header("stage3", 3, "Min / max summary", "STATEminmax(), Countryminmax(), CountryminmaxTable()"),

    hidden(div(
      id = "stage3_body",
      class = "glass-card",

      actionButton("sec_minmax", "Compute min / max", icon = icon("calculator"), class = "btn-glass"),

      tags$hr(),

      tableOutput("sec_minmax_tbl")
    )),


    br(),

    # =========================================================================
    # STAGE 4 — CREDIT / PENALTY  (single plot, full control strip)
    # =========================================================================

    sec_stage_header("stage4", 4, "Credit / penalty", "Credit_Penalty() — modifiers with >10% sensitivity"),

    hidden(div(
      id = "stage4_body",
      class = "glass-card",

      actionButton("sec_credit", "Generate credit / penalty chart", icon = icon("play"), class = "btn-glass"),

      tags$hr(),

      uiOutput("sec_credit_card")
    )),

    br(),

    # =========================================================================
    # STAGE 5 — STATE SENSITIVITY  (all states x LOB, one by one)
    # =========================================================================

    sec_stage_header("stage5", 5, "State sensitivity", "STATE_plot() for every state and LOB, rendered one by one"),

    hidden(div(
      id = "stage5_body",
      class = "glass-card",

      actionButton("sec_state_plots", "Generate all state plots", icon = icon("play"), class = "btn-glass"),

      tags$hr(),

      uiOutput("sec_stage5_gallery_controls"),

      uiOutput("sec_stage5_gallery")
    )),

    br(),

    # =========================================================================
    # STAGE 6 — MODIFIER DETAIL  (every modifier, one by one)
    # =========================================================================

    sec_stage_header("stage6", 6, "Modifier detail", "indmod() for every modifier, rendered one by one"),

    hidden(div(
      id = "stage6_body",
      class = "glass-card",

      actionButton("sec_modifier_plots", "Generate all modifier plots", icon = icon("play"), class = "btn-glass"),

      tags$hr(),

      uiOutput("sec_stage6_gallery_controls"),

      uiOutput("sec_stage6_gallery")
    )),

    br(),

    # hidden proxy download button used by the per-plot "download" icons
    # in stage 5 / stage 6 galleries (see server for the click -> download wiring)
    downloadButton("sec_gallery_download", "", style = "display:none;")
  )
}
