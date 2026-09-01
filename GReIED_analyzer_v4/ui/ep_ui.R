# =============================================================================
# ui/ep_ui.R
# EP Analytics subtab - preview-card panels (Meta_styles.css).
# =============================================================================

ep_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "preview-card",
      fluidRow(
        column(2, selectInput(ns("state"), "State", character())),
        column(2, selectInput(ns("loss"), "Loss perspective", character())),
        column(2, selectInput(ns("lob"), "LOB", character())),
        column(2, selectInput(ns("type"), "EP type", character())),
        column(2, selectInput(ns("rp"), "Return period", character())),
        column(
          2,
          checkboxGroupInput(
            ns("selected_perils"),
            "Subperils",
            choices = c("TC" = "TC", "SU" = "SU", "PF" = "PF", "IF" = "IF"),
            selected = c("TC", "SU", "PF")
          )
        )
      )
    ),

    uiOutput(ns("kpis")),

    fluidRow(
      column(
        5,
        div(
          class = "preview-card",
          h4("Subperil contribution at selected RP"),
          plotlyOutput(ns("pie"), height = "430px")
        )
      ),
      column(
        7,
        div(
          class = "preview-card",
          h4("EP curves by subperil"),
          plotlyOutput(ns("curves"), height = "430px")
        )
      )
    ),

    div(
      class = "preview-card",
      h4("State contribution profile at selected RP"),
      plotlyOutput(ns("states"), height = "900px")
    )
  )
}
