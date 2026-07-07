library(shiny)

cart_server <- function(
    input,
    output,
    session,
    cart
) {
  
  output$cart_table <- renderTable({
    
    items <- cart()
    
    if(length(items) == 0){
      
      return(
        data.frame(
          Status = "Cart Empty"
        )
      )
      
    }
    
    data.frame(
      
      Item = names(items),
      
      stringsAsFactors = FALSE
    )
    
  })
  
}