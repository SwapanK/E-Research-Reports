library(shiny)
library(shinyjs)
library(ggplot2)
library(colourpicker)

# -----------------------------------------------------------------------------
# SOURCE MODULES (including cart_utils for generate_item_id)
# -----------------------------------------------------------------------------
source("module/cart_utils.R")          # defines generate_item_id(), load/save functions (but save not used)
source("module/ppt_generator.R")
source("module/docx_generator.R")
source("module/rmd_generator.R")
source("module/function_Secmod.R")

# UI files
source("ui/vulnerability_ui.R")
source("ui/secmod_ui.R")
source("ui/cart_ui.R")

# Server files
source("server/vulnerability_server.R")
source("server/secmod_server.R")
source("server/cart_server.R")

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  title = "Vulnerability",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "toast_notification.js")
  ),
  uiOutput("app_ui")
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Fixed username (for reports only) ---
  username <- "User"
  
  # --- Session‑only cart ---
  cart <- reactiveVal(list())
  
  # --- Active page: start on Vulnerability ---
  active_page <- reactiveVal("vulnerability")
  
  # --- Navigation observers ---
  observeEvent(input$nav_vulnerability, active_page("vulnerability"))
  observeEvent(input$nav_secmod,        active_page("secmod"))
  observeEvent(input$nav_cart_icon,     active_page("cart"))
  
  # --- Module servers ---
  vulnerability_server(input, output, session, cart = cart, username = username)
  secmod_server(input, output, session, cart = cart, username = username)
  cart_server(input, output, session, cart = cart, username = username)
  
  # --- Cart badge ---
  output$cart_count <- renderUI({
    span(class = "cart-side-badge", length(cart()))
  })
  
  # --- Main UI ---
  output$app_ui <- renderUI({
    
    page_body <- switch(
      active_page(),
      "vulnerability" = vulnerability_ui(),
      "secmod"        = secmod_ui(),
      "cart"          = cart_ui(),
      vulnerability_ui()   # fallback
    )
    
    tagList(
      # ===== NAVBAR =====
      div(
        class = "navbar",
        # Left brand
        div(
          class = "navbar-brand",
          tags$img(src = "logo.png", class = "navbar-logo"),
          div(class = "vulsec-title", "Vulnerability")
        ),
        # Center navigation
        div(
          class = "navbar-nav",
          actionLink(
            "nav_vulnerability",
            label = tagList(icon("shield-alt"), span("Vulnerability")),
            class = if (active_page() == "vulnerability") "nav-link active" else "nav-link"
          ),
          actionLink(
            "nav_secmod",
            label = tagList(icon("chart-line"), span("Secondary Modifier")),
            class = if (active_page() == "secmod") "nav-link active" else "nav-link"
          )
        ),
        # Right side: Cart only (no user pill)
        div(
          class = "navbar-right-icons",
          div(
            class = "user-cart-pill",
            actionLink(
              "nav_cart_icon",
              label = tagList(
                div(
                  class = "cart-side",
                  icon("shopping-cart"),
                  uiOutput("cart_count")
                )
              ),
              class = "cart-link-fixed",
              title = "Cart"
            )
          )
        )
      ),
      # ===== PAGE CONTENT =====
      div(class = "main-page-container", page_body),
      # ===== TOAST CONTAINER =====
      div(class = "toast-container bottom-right")
    )
  })
}

shinyApp(ui, server)






