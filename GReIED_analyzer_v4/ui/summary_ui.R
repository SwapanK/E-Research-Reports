# =============================================================================
# ui/summary_ui.R
# Executive Summary subtab - KPI tiles (greied_extra.css) + preview-card
# panels (Meta_styles.css) hosting the donut and stacked contribution chart.
# =============================================================================

summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("kpis")),
    fluidRow(
      column(
        5,
        div(
          class = "preview-card",
          h4("Selected state contribution mix"),
          plotlyOutput(ns("donut"), height = "420px")
        )
      ),
      column(
        7,
        div(
          class = "preview-card",
          h4("State contribution profile"),
          plotlyOutput(ns("stack"), height = "780px")
        )
      )
    )
  )
}
