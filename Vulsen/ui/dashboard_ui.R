library(shiny)

dashboard_ui <- function() {
  
  fluidPage(
    
    fluidRow(
      
      column(
        12,
        
        div(
          
          class = "page-header",
          
          h1("Dashboard"),
          
          p(
            "Welcome to Vulnerability & Secondary Modifier Platform"
          )
        )
      )
    ),
    
    fluidRow(
      
      column(
        4,
        
        div(
          class = "stat-card",
          
          div(class = "stat-value",
              textOutput("vul_count")
          ),
          
          div(
            class = "stat-label",
            "Vulnerability Runs"
          )
        )
      ),
      
      column(
        4,
        
        div(
          class = "stat-card",
          
          div(
            class = "stat-value",
            textOutput("secmod_count")
          ),
          
          div(
            class = "stat-label",
            "Secondary Modifier Runs"
          )
        )
      ),
      
      column(
        4,
        
        div(
          class = "stat-card",
          
          div(
            class = "stat-value",
            textOutput("cart_count_text")
          ),
          
          div(
            class = "stat-label",
            "Items In Cart"
          )
        )
      )
    )
  )
}