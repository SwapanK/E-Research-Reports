# =============================================================================
# ui/preview_ui.R
# Data Preview subtab - uses .preview-card / .small-note from
# www/Meta_styles.css unmodified.
# =============================================================================

preview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "small-note",
      strong("Loaded data preview. "),
      "Use the filters above each column. Uploaded files update these tables immediately."
    ),

    fluidRow(
      column(
        6,
        div(
          class = "preview-card",
          h4("AAL Dataset"),
          div(class = "small-note", textOutput(ns("aal_meta"))),
          DTOutput(ns("aal_table"))
        )
      ),

      column(
        6,
        div(
          class = "preview-card",
          h4("EP Dataset"),
          div(class = "small-note", textOutput(ns("ep_meta"))),
          DTOutput(ns("ep_table"))
        )
      )
    )
  )
}
