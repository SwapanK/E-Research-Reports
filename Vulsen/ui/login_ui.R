library(shiny)

login_ui <- function(){
  
  username <- toupper(Sys.info()[["user"]])
  
  div(
    class = "login-page black-splash",
    
    ## Stage 1: full black welcome screen
    div(
      class = "splash-stage stage-welcome",
      div(class = "welcome-word",     "Welcome"),
      div(class = "welcome-username", username)
    ),
    
    ## Stage 2: module name / logo screen
    div(
      class = "splash-stage stage-module",
      tags$img(src = "logo1.png", class = "splash-logo"),
      div(class = "splash-module-title", "Vulnerability & Secondary Modifier"),
      div(class = "splash-module-sub",   "Catastrophe Risk Analytics Platform")
    ),
    
    # Real Shiny input, just hidden — wires into your existing App.R logic
    actionButton("enter_app", label = NULL, style = "display:none;"),
    
    tags$script(HTML("
      setTimeout(function() {
        document.getElementById('enter_app').click();
      }, 5300);
    "))
  )
}
