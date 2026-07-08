library(shiny)
library(ggplot2)

# =============================================================================
# SECONDARY MODIFIER SERVER
# =============================================================================
# Mirrors vulnerability_server.R exactly, calling the same shared
# plotting_function.R with module = "secmod".
#
# cart      : shared reactiveVal(list()) from App.R
# username  : reactive() returning the current display username
# =============================================================================

secmod_server <- function(input, output, session, cart, username) {

  current_result <- reactiveVal(NULL)

  ## =========================================================================
  ## RUN ANALYSIS — calls the shared plotting_function.R
  ## =========================================================================

  observeEvent(input$run_secmod, {

    result <- generate_plot_commentary(
      module = "secmod",
      lob    = input$sec_lob,
      state  = input$sec_state
    )

    current_result(result)

    session$sendCustomMessage(
      "show-toast",
      list(
        text     = "Secondary Modifier analysis generated",
        type     = "success",
        icon     = "fa-chart-line",
        duration = 2500
      )
    )
  })

  ## =========================================================================
  ## PLOT (rendered inside the combined result card below)
  ## =========================================================================

  output$sec_plot <- renderPlot({
    req(current_result())
    current_result()$plot
  })

  ## =========================================================================
  ## COMBINED RESULT CARD — plot + commentary together, cart-icon button
  ## in the header, matching the Cart page's item-card layout.
  ## =========================================================================

  output$sec_result_card <- renderUI({

    res <- current_result()

    div(
      class = "cart-item-card",

      div(
        class = "cart-item-header",

        div(
          span(class = "cart-item-badge", "Secondary Modifier"),
          if (!is.null(res)) {
            tags$span(
              class = "cart-item-time",
              style = "margin-left:10px;",
              format(res$timestamp, "%Y-%m-%d %H:%M")
            )
          }
        ),

        actionButton(
          inputId = "add_sec",
          label   = NULL,
          icon    = icon("cart-plus"),
          class   = "btn-icon-cart",
          title   = "Add to Cart"
        )
      ),

      if (is.null(res)) {
        div(
          class = "empty-state",
          icon("chart-area"),
          p("Run an analysis to generate a plot and commentary.")
        )
      } else {
        tagList(
          plotOutput("sec_plot", height = "380px"),
          div(class = "commentary-text", style = "margin-top:16px;", res$commentary)
        )
      }
    )
  })

  ## =========================================================================
  ## ADD TO CART — only the plot + commentary (if available) are saved
  ## =========================================================================

  observeEvent(input$add_sec, {

    res <- current_result()

    if (is.null(res) || is.null(res$plot)) {
      session$sendCustomMessage(
        "show-toast",
        list(
          text     = "Run an analysis before adding to cart",
          type     = "error",
          icon     = "fa-exclamation-circle",
          duration = 3000
        )
      )
      return()
    }

    item <- list(
      id         = generate_item_id(),
      module     = "Secondary Modifier",
      plot       = res$plot,
      commentary = if (!is.null(res$commentary) && nzchar(res$commentary)) res$commentary else NULL,
      timestamp  = Sys.time()
    )

    current_cart <- cart()
    current_cart[[length(current_cart) + 1]] <- item
    cart(current_cart)

    save_cart(username(), current_cart)

    session$sendCustomMessage(
      "show-toast",
      list(
        text     = "Secondary Modifier item added to cart",
        type     = "success",
        icon     = "fa-cart-plus",
        duration = 2500
      )
    )
  })
}
