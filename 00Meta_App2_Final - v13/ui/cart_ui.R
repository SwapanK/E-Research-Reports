library(shiny)

# =============================================================================
# CART UI - MODULE VERSION (with unified header banner)
# =============================================================================
cart_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "app-shell",
      
      # ---- Unified header banner (same as Input/Result pages) ----
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Cart")
        ),
        div(
          class = "header-sub",
          p("Review your saved plots and commentary, remove items, or export everything to the report format of your choice.")
        )
      ),
      
      # ---- Main content (scrollable) ----
      div(
        class = "single-scroll-panel",
        
        div(
          class = "glass-card",
          div(
            style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:15px;",
            
            div(
              h3(class = "gradient-text", "Saved Items"),
              tags$small(style = "color:#718096;", uiOutput(ns("cart_summary_text"), inline = TRUE))
            ),
            
            div(
              style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
              
              tags$button(
                id = ns("cart_remove_all"),
                class = "btn btn-danger btn-sm action-button",
                icon("trash-alt"),
                "Remove All",
                style = "background: #EF4444; border-color: #EF4444; color: #fff; font-weight: 700;"
              ),
              
              ## ---- Format choice: user picks PPT / DOCX / HTML before exporting ----
              div(
                style = "min-width:190px;",
                selectInput(
                  inputId  = ns("export_format"),
                  label    = NULL,
                  choices  = c(
                    "PowerPoint (.pptx)" = "pptx",
                    "Word (.docx)"       = "docx",
                    "HTML (.html)"       = "html"
                  ),
                  selected = "pptx",
                  width    = "100%"
                )
              ),
              
              actionButton(
                ns("generate_report_btn"),
                "Generate Report",
                icon  = icon("file-export"),
                class = "btn-glass"
              )
            )
          )
        ),
        
        # ---- JS trigger for download ----
        tags$script(HTML("
          if (!window.__cartDownloadHandlerRegistered) {
            window.__cartDownloadHandlerRegistered = true;
            Shiny.addCustomMessageHandler('cart-trigger-download', function(msg) {
              var a = document.createElement('a');
              a.href = msg.url;
              a.download = msg.filename;
              document.body.appendChild(a);
              a.click();
              document.body.removeChild(a);
            });
          }
        ")),
        
        # ---- Cart items list ----
        uiOutput(ns("cart_items_ui"))
      )
    )
  )
}