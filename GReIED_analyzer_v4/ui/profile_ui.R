# =============================================================================
# ui/profile_ui.R
# State Profile subtab - preview-card panels (Meta_styles.css).
# =============================================================================

profile_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        7,
        div(
          class = "preview-card",
          h4("AAL by peril"),
          plotlyOutput(ns("bar"), height = "430px")
        )
      ),
      column(
        5,
        div(
          class = "preview-card",
          h4("Contribution table"),
          DTOutput(ns("tab"))
        )
      )
    ),
    div(
      class = "preview-card",
      h4("GU versus GR by peril"),
      plotlyOutput(ns("gr"), height = "400px")
    )
  )
}
