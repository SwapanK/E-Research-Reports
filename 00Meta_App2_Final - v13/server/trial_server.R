# server/trial_server.R
# Trial server – reacts to Run button

trialServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Combine the four inputs into a single string
    combined_text <- eventReactive(input$run, {
      paste(
        "Input 1:", input$input1,
        "Input 2:", input$input2,
        "Input 3:", input$input3,
        "Input 4:", input$input4,
        sep = "\n"
      )
    })
    
    output$output_text <- renderText({
      combined_text()
    })
    
  })
}








