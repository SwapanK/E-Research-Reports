
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

# ---- VulSen (Vulnerability Sensitivity) utils - source before its ui/server ----
source("R/VulsenAPP_utils/VulsenAPP_config.R")
source("R/VulsenAPP_utils/VulsenAPP_data_logic.R")
source("R/VulsenAPP_utils/VulsenAPP_html_report.R")
source("R/VulsenAPP_utils/VulsenAPP_plot_functions.R")
source("R/VulsenAPP_utils/VulsenAPP_ui_helpers.R")

source("ui/input_preparation_ui.R")
source("ui/result_extraction_ui.R")
source("ui/secmod_ui.R")        
source("ui/cart_ui.R")
source("ui/home_ui.R")          
source("ui/VulSen_ui.R")
source("ui/user_guide_ui.R")

source("server/input_preparation_server.R")
source("server/result_extraction_server.R")
source("server/secmod_server.R")        
source("server/cart_server.R")
source("server/home_server.R")          
source("server/Vulsen_server.R")




source("ui/trial_ui.R")
source("server/trial_server.R")






# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  
  # ----- HEADER (single unified bar: hamburger + app/page name + cart + logo) -----
  dashboardHeader(
    title = NULL,
    # Left: app name
    tags$li(
      class = "dropdown header-left-item",
      tags$div(
        class = "header-left",
        span(class = "header-app-name", "Vulnerability Module")
      )
    ),
    # Right: cart -> vertical divider -> email contact -> vertical divider -> logo
    tags$li(
      class = "dropdown header-right-item",
      tags$div(
        class = "header-right",
        uiOutput("header_cart_ui"),
        tags$div(class = "header-divider"),
        tags$a(
          href = "mailto:nikilpujari@gmail.com,swapanmasanta@gmail.com",
          target = "_blank",
          title = "Email Nikil Pujari / Swapan Masanta",
          class = "header-contact-link",
          icon("envelope"),
          span("Nikil Pujari / Swapan Masanta")
        ),
        tags$div(class = "header-divider"),
        tags$img(class = "header-logo-img", src = "logo-removebg-preview.png")
      )
    )
  ),
  
  # ----- SIDEBAR (collapsible; Cart now added back to sidebar) -----
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
      tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),
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
  
  # --- Sync sidebar highlight with active_page (Problem 10) ---
  observeEvent(active_page(), {
    updateTabItems(session, "sidebar", selected = active_page())
  }, ignoreInit = FALSE)
  
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
  
  # --- Sidebar menu (Cart added back, Problem 9) ---
  output$sidebar_menu <- renderMenu({
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",               tabName = "home",          icon = icon("house")),
      menuItem("Input File Creation", tabName = "input_prep",   icon = icon("location-arrow")),
      menuItem("Result Extraction",   tabName = "result_extract", icon = icon("database")),
      menuItem("Vulnerability Sensitivity", tabName = "vulsen", icon = icon("chart-column")),  # <-- ADDED, before Secmod
      menuItem("Secondary Modifier", tabName = "secmod",        icon = icon("shield-alt")),
      menuItem("Cart",               tabName = "cart",          icon = icon("shopping-cart")),  # <-- ADDED
      menuItem("User Guide",         tabName = "user_guide",    icon = icon("book")),  # <-- ADDED, last page
      
      
      menuItem("Trial", tabName = "trial", icon = icon("flask"))
      
    )
  })
  
  # --- Module servers ---
  home_nav <- callModule(homeServer, "home", cart = cart)
  callModule(inputPrepServer, "prep")
  callModule(resultExtractServer, "extract")
  callModule(secmod_server, "secmod", cart = cart, username = username)
  callModule(cart_server, "cart", cart = cart, username = username)
  
  trialServer("trial")
  
  # Vulsen_server is written with the newer moduleServer() pattern
  # internally (Vulsen_server <- function(id) { moduleServer(id, ...) }),
  # so it's invoked directly - NOT via callModule(), which would wrap it a
  # second time and pass it the wrong arguments.
  Vulsen_server("vulsen", cart = cart, username = username)
  
  # --- Home page cards/buttons drive navigation via a returned reactiveVal ---
  observeEvent(home_nav(), {
    if (!is.null(home_nav())) active_page(home_nav())
  })
  
  # --- User Guide PDF download (static asset served straight from www/) ---
  output$download_user_guide <- downloadHandler(
    filename = function() "Vulnerability_App_User_Guide.pdf",
    content = function(file) {
      file.copy("www/Vulnerability_App_User_Guide.pdf", file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  # --- Main UI (only the page body, navbar is now in dashboardHeader/Sidebar) ---
  output$app_ui <- renderUI({
    switch(
      active_page(),
      "home"             = homeUI("home"),
      "input_prep"       = inputPrepUI("prep"),
      "result_extract"   = resultExtractUI("extract"),
      "vulsen"           = VulSen_ui("vulsen"),
      "secmod"           = secmod_ui("secmod"),
      "cart"             = cart_ui("cart"),
      "user_guide"       = userGuideUI(),
      "trial" = trialUI("trial"),
      homeUI("home")         # fallback
    )
  })
}

shinyApp(ui, server)
