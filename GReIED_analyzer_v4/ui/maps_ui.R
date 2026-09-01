# =============================================================================
# ui/maps_ui.R
# Geo Analytics subtab - preview-card panels (Meta_styles.css).
# =============================================================================

maps_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "preview-card",
      fluidRow(
        column(
          2,
          selectInput(ns("source"), "Source", c("AAL", "EP"))
        ),
        column(
          2,
          selectInput(
            ns("metric"),
            "Subperil",
            choices = NULL
          )
        ),
        column(
          2,
          selectInput(ns("loss"), "Loss perspective", c("GU", "GR"))
        ),
        column(
          2,
          selectInput(
            ns("lob"),
            "LOB",
            c("Total", "Commercial", "Personal")
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = paste0("input['", ns("source"), "']=='EP'"),
            selectInput(ns("type"), "EP Type", character())
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = paste0("input['", ns("source"), "']=='EP'"),
            selectInput(ns("rp"), "Return Period", character())
          )
        )
      ),
      fluidRow(
        column(
          3,
          checkboxInput(ns("log_scale"), "Use log scale", TRUE)
        )
      )
    ),

    fluidRow(
      column(
        6,
        div(
          class = "preview-card",
          h4("Value by State"),
          plotlyOutput(ns("value"), height = "560px")
        )
      ),
      column(
        6,
        div(
          class = "preview-card",
          h4("Subperil Contribution by State"),
          plotlyOutput(ns("share"), height = "560px")
        )
      )
    )
  )
}
