library(shiny)

cart_ui <- function() {
  
  fluidPage(
    
    div(
      class = "page-header",
      
      h1("Cart"),
      
      p("Saved items and future exports")
    ),
    
    div(
      
      class = "glass-card",
      
      h3("Cart Contents"),
      
      br(),
      
      tableOutput("cart_table")
    )
  )
}