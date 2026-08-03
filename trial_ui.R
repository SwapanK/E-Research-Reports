# ui/trial_ui.R
# Trial page – left: inputs, right: output

trialUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      class = "app-shell",
      
      # ---- Header (same style as other modules) ----
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Trial Page")
        ),
        div(
          class = "header-sub",
          p("Enter four text values and click Run to see the combined result.")
        )
      ),
      
      # ---- Two‑column body ----
      div(
        class = "app-body",
        
        # Left panel – inputs
        div(
          class = "left-scroll-panel",
          div(
            class = "sidebar-card",
            h4("Inputs"),
            textInput(ns("input1"), "Input 1", value = ""),
            textInput(ns("input2"), "Input 2", value = ""),
            textInput(ns("input3"), "Input 3", value = ""),
            textInput(ns("input4"), "Input 4", value = ""),
            br(),
            actionButton(ns("run"), "Run", class = "create-btn")
          )
        ),
        
        # Right panel – output
        div(
          class = "right-panel",
          div(
            class = "info-panel",
            style = "min-height: 200px;",
            h4("Output"),
            verbatimTextOutput(ns("output_text"))
          )
        )
      )
    )
  )
}






