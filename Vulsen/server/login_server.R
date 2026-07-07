library(shiny)

login_server <- function(
    input,
    output,
    session,
    logged_in
) {
  
  observeEvent(
    input$enter_app,
    {
      
      logged_in(TRUE)
      
      session$sendCustomMessage(
        "show-toast",
        list(
          text = "Welcome to Vul/Sec",
          type = "success"
        )
      )
      
    }
  )
  
}