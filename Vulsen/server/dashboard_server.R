library(shiny)

dashboard_server <- function(
    input,
    output,
    session,
    username
) {
  
  output$vul_count <- renderText({
    "0"
  })
  
  output$secmod_count <- renderText({
    "0"
  })
  
  output$cart_count_text <- renderText({
    "0"
  })
}