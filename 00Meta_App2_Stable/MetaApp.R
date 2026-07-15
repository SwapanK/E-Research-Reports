# MetaApp.R - Combined Shiny Application

options(shiny.maxRequestSize = 1024 * 1024^2)

# Load required packages
library(shiny)
library(shinyjs)
library(data.table)
library(dplyr)
library(DT)
library(RODBC)
library(glue)
library(stringr)
library(ggplot2)
library(colourpicker)

# -----------------------------------------------------------------------------
# SOURCE ALL MODULES (including cart, secmod and home)
# -----------------------------------------------------------------------------

source("R/input_preparation_helpers.R")   
source("R/result_extraction_helpers.R")  
source("R/function_Secmod.R")  

source("R/cart_utils.R")                 # generate_item_id, etc.
source("R/docx_generator.R")
source("R/ppt_generator.R")
source("R/rmd_generator.R") 

source("ui/input_preparation_ui.R")
source("ui/result_extraction_ui.R")
source("ui/secmod_ui.R")        # now defines secmod_ui(id) - module UI
source("ui/cart_ui.R")
source("ui/home_ui.R")          # defines homeUI(id) - module UI

source("server/input_preparation_server.R")
source("server/result_extraction_server.R")
source("server/secmod_server.R")        # now defines secmod_server() - module server
source("server/cart_server.R")
source("server/home_server.R")          # defines homeServer() - module server

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  title = "Vulsens & Secmod Toolkit",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Meta_styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "home_style.css"),
    tags$script(src = "toast_notification.js")
  ),
  uiOutput("app_ui")
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Fixed username (used for reports) ---
  username <- toupper(Sys.info()[["user"]])
  
  # --- Session-only cart ---
  cart <- reactiveVal(list())
  
  # --- Active page: start on Home ---
  active_page <- reactiveVal("home")
  
  # --- Navigation observers ---
  observeEvent(input$nav_home, active_page("home"))
  observeEvent(input$nav_input_prep, active_page("input_prep"))
  observeEvent(input$nav_result_extract, active_page("result_extract"))
  observeEvent(input$nav_secmod, active_page("secmod"))
  observeEvent(input$nav_cart_icon, active_page("cart"))
  
  # --- Module servers ---
  home_nav <- callModule(homeServer, "home", cart = cart)
  callModule(inputPrepServer, "prep")
  callModule(resultExtractServer, "extract")
  
  # *** FIX: Use callModule with the module ID "secmod" ***
  callModule(secmod_server, "secmod", cart = cart, username = username)
  
  callModule(cart_server, "cart", cart = cart, username = username)
  
  # --- Home page cards/buttons drive navigation via a returned reactiveVal ---
  observeEvent(home_nav(), {
    if (!is.null(home_nav())) active_page(home_nav())
  })
  
  # --- Cart badge ---
  output$cart_count <- renderUI({
    span(class = "cart-side-badge", length(cart()))
  })
  
  # --- Main UI ---
  output$app_ui <- renderUI({
    
    page_body <- switch(
      active_page(),
      "home"             = homeUI("home"),
      "input_prep"       = inputPrepUI("prep"),
      "result_extract"   = resultExtractUI("extract"),
      # *** FIX: Call secmod_ui with the same module ID "secmod" ***
      "secmod"           = secmod_ui("secmod"),
      "cart"             = cart_ui("cart"),   # <-- pass module ID
      homeUI("home")         # fallback
    )
    
    tagList(
      # ===== CUSTOM NAVBAR =====
      div(
        class = "navbar",
        # Left brand
        div(
          class = "navbar-brand",
          tags$img(src = "GallagherRe_StackedLarge-3D.png", class = "navbar-logo"),
          div(class = "vulsec-title", "Vulsens & Secmod")
        ),
        # Center navigation
        div(
          class = "navbar-nav",
          actionLink(
            "nav_home",
            label = tagList(icon("house"), span("Home")),
            class = if (active_page() == "home") "nav-link active" else "nav-link"
          ),
          actionLink(
            "nav_input_prep",
            label = tagList(icon("location-arrow"), span("Input File Creation")),
            class = if (active_page() == "input_prep") "nav-link active" else "nav-link"
          ),
          actionLink(
            "nav_result_extract",
            label = tagList(icon("database"), span("Result Extraction")),
            class = if (active_page() == "result_extract") "nav-link active" else "nav-link"
          ),
          actionLink(
            "nav_secmod",
            label = tagList(icon("shield-alt"), span("Secondary Modifier")),
            class = if (active_page() == "secmod") "nav-link active" else "nav-link"
          )
        ),
        # Right side: Cart icon
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
