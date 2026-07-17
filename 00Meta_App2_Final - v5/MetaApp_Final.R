# MetaApp.R - Combined Shiny Application

options(shiny.maxRequestSize = 1024 * 1024^2)

# Load required packages
library(shiny)
library(shinydashboard)      # <-- sidebar layout
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
# SOURCE ALL MODULES (unchanged)
# -----------------------------------------------------------------------------

source("R/input_preparation_helpers.R")   
source("R/result_extraction_helpers.R")  
source("R/function_Secmod.R")  

source("R/cart_utils.R")                 
source("R/docx_generator.R")
source("R/ppt_generator.R")
source("R/rmd_generator.R") 

source("ui/input_preparation_ui.R")
source("ui/result_extraction_ui.R")
source("ui/secmod_ui.R")        
source("ui/cart_ui.R")
source("ui/home_ui.R")          

source("server/input_preparation_server.R")
source("server/result_extraction_server.R")
source("server/secmod_server.R")        
source("server/cart_server.R")
source("server/home_server.R")          

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  
  # ----- HEADER (single unified bar: hamburger + brand + header cart icon) -----
  dashboardHeader(
    title = NULL,
    tags$li(
      class = "dropdown header-brand-item",
      tags$div(
        class = "header-brand",
        tags$img(src = "logo-removebg-preview.png"),
        span("Vulsens & Secmod")
      )
    ),
    tags$li(
      class = "dropdown header-cart-item",
      uiOutput("header_cart_ui")
    )
  ),
  
  # ----- SIDEBAR (collapsible; Cart lives only in the header now) -----
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenuOutput("sidebar_menu")
  ),
  
  # ----- BODY (same CSS + page content) -----
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Meta_styles.css"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "dashboard_style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),   # <-- ADD THIS LINE
      tags$script(src = "toast_notification.js") 
    ),
    uiOutput("app_ui"),          # renders the active module
    div(class = "toast-container bottom-right")
  )
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
  
  # --- Navigation observers (sidebar clicks) ---
  observeEvent(input$sidebar, {
    active_page(input$sidebar)
  })
  
  # --- Header cart button click -> same navigation as the old sidebar item ---
  observeEvent(input$header_cart_btn, {
    active_page("cart")
  })
  
  # --- Header cart icon (icon-only, live reactive count badge) ---
  output$header_cart_ui <- renderUI({
    n <- length(cart())
    actionButton(
      "header_cart_btn",
      label = tagList(
        icon("shopping-cart"),
        if (n > 0) tags$span(class = "header-cart-badge", n)
      ),
      class = "header-cart-btn",
      title = "Cart"
    )
  })
  
  # --- Sidebar menu (Cart item removed; now only in the header) ---
  output$sidebar_menu <- renderMenu({
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",               tabName = "home",          icon = icon("house")),
      menuItem("Input File Creation", tabName = "input_prep",   icon = icon("location-arrow")),
      menuItem("Result Extraction",   tabName = "result_extract", icon = icon("database")),
      menuItem("Secondary Modifier", tabName = "secmod",        icon = icon("shield-alt"))
    )
  })
  
  # --- Module servers ---
  home_nav <- callModule(homeServer, "home", cart = cart)
  callModule(inputPrepServer, "prep")
  callModule(resultExtractServer, "extract")
  callModule(secmod_server, "secmod", cart = cart, username = username)
  callModule(cart_server, "cart", cart = cart, username = username)
  
  # --- Home page cards/buttons drive navigation via a returned reactiveVal ---
  observeEvent(home_nav(), {
    if (!is.null(home_nav())) active_page(home_nav())
  })
  
  # --- Main UI (only the page body, navbar is now in dashboardHeader/Sidebar) ---
  output$app_ui <- renderUI({
    switch(
      active_page(),
      "home"             = homeUI("home"),
      "input_prep"       = inputPrepUI("prep"),
      "result_extract"   = resultExtractUI("extract"),
      "secmod"           = secmod_ui("secmod"),
      "cart"             = cart_ui("cart"),
      homeUI("home")         # fallback
    )
  })
}

shinyApp(ui, server)



