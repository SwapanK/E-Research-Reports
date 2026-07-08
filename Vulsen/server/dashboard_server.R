library(shiny)

# =============================================================================
# DASHBOARD SERVER
# =============================================================================
# username : reactive() returning the current display username
# cart     : shared reactiveVal(list()) from App.R
# =============================================================================

dashboard_server <- function(input, output, session, username, cart) {

  output$vul_count <- renderText({
    items <- cart()
    as.character(sum(vapply(items, function(x) identical(x$module, "Vulnerability"), logical(1))))
  })

  output$secmod_count <- renderText({
    items <- cart()
    as.character(sum(vapply(items, function(x) identical(x$module, "Secondary Modifier"), logical(1))))
  })

  output$cart_count_text <- renderText({
    as.character(length(cart()))
  })
}
