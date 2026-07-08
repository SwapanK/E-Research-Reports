library(shiny)

# =============================================================================
# SECONDARY MODIFIER UI
# =============================================================================
# Mirrors vulnerability_ui.R exactly in structure/styling so both pages feel
# like the same product. Both call the shared plotting_function.R.
# =============================================================================

secmod_ui <- function() {

  fluidPage(

    # =========================================================================
    # HEADER
    # =========================================================================

    div(
      class = "page-header",

      h1(class = "gradient-text", "Secondary Modifier"),

      p("Generate secondary modifier impact charts, review commentary, and save results to your cart.")
    ),

    # =========================================================================
    # WORKFLOW OVERVIEW CARDS
    # =========================================================================

    fluidRow(

      column(4,
        div(
          class = "glass-card",
          div(style = "font-size:35px; color:#3B82F6; margin-bottom:10px;", icon("sliders-h")),
          h4("Modifier Factors"),
          p("Review individual secondary modifier adjustments.")
        )
      ),

      column(4,
        div(
          class = "glass-card",
          div(style = "font-size:35px; color:#764ba2; margin-bottom:10px;", icon("balance-scale")),
          h4("Rate Sensitivity"),
          p("See which characteristics drive the largest rate impact.")
        )
      ),

      column(4,
        div(
          class = "glass-card",
          div(style = "font-size:35px; color:#667eea; margin-bottom:10px;", icon("layer-group")),
          h4("Portfolio Application"),
          p("Understand how modifiers apply across the portfolio.")
        )
      )
    ),

    # =========================================================================
    # ANALYSIS CONTROLS
    # =========================================================================

    div(
      class = "glass-card",

      h3(class = "gradient-text", "Secondary Modifier Workspace"),
      tags$small(style = "color:#718096;", "Secondary rating factor review environment"),

      tags$hr(),

      fluidRow(

        column(4,
          selectInput(
            "sec_lob",
            "Line of Business",
            choices = c("Residential", "Commercial", "Industrial")
          )
        ),

        column(4,
          selectInput(
            "sec_state",
            "State",
            choices = c("All States")
          )
        ),

        column(4,
          br(),
          actionButton(
            "run_secmod",
            "Run Analysis",
            icon  = icon("play"),
            class = "btn-glass"
          )
        )
      )
    ),

    # =========================================================================
    # RESULT CARD — plot + commentary together, cart-icon "Add To Cart"
    # =========================================================================

    uiOutput("sec_result_card")
  )
}
