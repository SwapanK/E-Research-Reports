library(shiny)
library(shinyjs)

## =============================================================================
## SOURCE UI FILES
## =============================================================================

source("ui/login_ui.R")
source("ui/dashboard_ui.R")
source("ui/vulnerability_ui.R")
source("ui/secmod_ui.R")
source("ui/cart_ui.R")

## =============================================================================
## UI
## =============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    
    tags$script(
      src = "toast_notification.js"
    )
  ),
  
  uiOutput("app_ui")
)

## =============================================================================
## SERVER
## =============================================================================

server <- function(input, output, session) {
  ## =========================================================================
  ## USER
  ## =========================================================================
  
  username <- reactiveVal(
    toupper(Sys.info()[["user"]])
  )
  
  display_user <- reactive({
    x <- username()
    
    if (is.null(x) || is.na(x) || x == "") {
      return("SRMASANTA")
    }
    
    x
  })
  
  ## =========================================================================
  ## LOGIN STATE
  ## =========================================================================
  
  logged_in <- reactiveVal(FALSE)
  
  ## =========================================================================
  ## ACTIVE PAGE
  ## =========================================================================
  
  active_page <- reactiveVal("dashboard")
  
  ## =========================================================================
  ## CART
  ## =========================================================================
  
  cart <- reactiveVal(list())
  
  ## =========================================================================
  ## LOGIN EVENT
  ## =========================================================================
  
  observeEvent(input$enter_app, {
    logged_in(TRUE)
    active_page("dashboard")
  })
  
  ## =========================================================================
  ## LOGOUT EVENT
  ## =========================================================================
  
  observeEvent(input$logout, {
    
    session$sendCustomMessage(
      "show-goodbye",
      list(username = display_user())
    )
    
    shinyjs::delay(4400, {
      stopApp()
    })
  })
  
  ## =========================================================================
  ## NAVIGATION EVENTS
  ## =========================================================================
  
  observeEvent(input$nav_dashboard, {
    active_page("dashboard")
  })
  
  observeEvent(input$nav_vulnerability, {
    active_page("vulnerability")
  })
  
  observeEvent(input$nav_secmod, {
    active_page("secmod")
  })
  
  observeEvent(input$nav_cart_icon, {
    active_page("cart")
  })
  
  ## =========================================================================
  ## DASHBOARD COUNTS
  ## =========================================================================
  
  output$vul_count <- renderText({
    "0"
  })
  
  output$secmod_count <- renderText({
    "0"
  })
  
  output$cart_count_text <- renderText({
    length(cart())
  })
  
  ## =========================================================================
  ## CART BADGE
  ## =========================================================================
  
  output$cart_count <- renderUI({
    span(
      class = "cart-side-badge",
      length(cart())
    )
  })
  
  ## =========================================================================
  ## CART TABLE
  ## =========================================================================
  
  output$cart_table <- renderTable({
    
    current_cart <- cart()
    
    if (length(current_cart) == 0) {
      return(
        data.frame(
          Message = "No items in cart"
        )
      )
    }
    
    do.call(
      rbind,
      lapply(
        current_cart,
        as.data.frame
      )
    )
  })
  
  ## =========================================================================
  ## VULNERABILITY - ADD TO CART
  ## =========================================================================
  
  observeEvent(input$add_vul, {
    
    current_cart <- cart()
    
    current_cart[[length(current_cart) + 1]] <- list(
      Module = "Vulnerability",
      Item = "Vulnerability Run",
      Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    cart(current_cart)
    
    session$sendCustomMessage(
      "show-toast",
      list(
        text = "Vulnerability item added to cart",
        type = "success",
        icon = "fa-check-circle",
        duration = 3000
      )
    )
  })
  
  ## =========================================================================
  ## SECMOD - ADD TO CART
  ## =========================================================================
  
  observeEvent(input$add_sec, {
    
    current_cart <- cart()
    
    current_cart[[length(current_cart) + 1]] <- list(
      Module = "Secondary Modifier",
      Item = "Secondary Modifier Run",
      Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    cart(current_cart)
    
    session$sendCustomMessage(
      "show-toast",
      list(
        text = "Secondary Modifier item added to cart",
        type = "success",
        icon = "fa-check-circle",
        duration = 3000
      )
    )
  })
  
  ## =========================================================================
  ## DASHBOARD BUTTONS
  ## =========================================================================
  
  observeEvent(input$goto_vulnerability, {
    active_page("vulnerability")
  })
  
  observeEvent(input$goto_secmod, {
    active_page("secmod")
  })
  
  ## =========================================================================
  ## MAIN UI
  ## =========================================================================
  
  output$app_ui <- renderUI({
    
    ## ---------------------------------------------------------------------
    ## LOGIN PAGE
    ## ---------------------------------------------------------------------
    
    if (!logged_in()) {
      return(
        login_ui()
      )
    }
    
    ## ---------------------------------------------------------------------
    ## PAGE BODY
    ## ---------------------------------------------------------------------
    
    page_body <- switch(
      active_page(),
      
      "dashboard" = dashboard_ui(),
      "vulnerability" = vulnerability_ui(),
      "secmod" = secmod_ui(),
      "cart" = cart_ui(),
      
      dashboard_ui()
    )
    
    ## ---------------------------------------------------------------------
    ## MAIN APPLICATION LAYOUT
    ## ---------------------------------------------------------------------
    
    tagList(
      
      ## =================================================================
      ## TOP NAVBAR
      ## =================================================================
      
      div(
        class = "navbar",
        
        ## -------------------------------------------------------------
        ## LEFT BRAND
        ## -------------------------------------------------------------
        
        div(
          class = "navbar-brand",
          
          tags$img(
            src = "logo.png",
            class = "navbar-logo"
          ),
          
          div(
            class = "vulsec-title",
            "Vul & Sec"
          )
        ),
        
        ## -------------------------------------------------------------
        ## CENTER NAVIGATION
        ## -------------------------------------------------------------
        
        div(
          class = "navbar-nav",
          
          actionLink(
            inputId = "nav_dashboard",
            label = tagList(
              icon("home"),
              span("Dashboard")
            ),
            class = ifelse(
              active_page() == "dashboard",
              "nav-link active",
              "nav-link"
            )
          ),
          
          actionLink(
            inputId = "nav_vulnerability",
            label = tagList(
              icon("shield-alt"),
              span("Vulnerability")
            ),
            class = ifelse(active_page() == "vulnerability",
                           "nav-link active",
                           "nav-link"
            )
          ),
          
          actionLink(
            inputId = "nav_secmod",
            label = tagList(
              icon("chart-line"),
              span("Secondary Modifier")
            ),
            class = ifelse(
              active_page() == "secmod",
              "nav-link active",
              "nav-link"
            )
          )
        ),
        
        ## -------------------------------------------------------------
        ## RIGHT SIDE FIXED CART + USER DROPDOWN
        ## -------------------------------------------------------------
        
        div(
          class = "navbar-right-icons",
          
          div(
            class = "user-cart-pill",
            
            actionLink(
              inputId = "nav_cart_icon",
              label = tagList(
                div(
                  class = "cart-side",
                  icon("shopping-cart"),
                  uiOutput("cart_count")
                )
              ),
              class = "cart-link-fixed",
              title = "Cart"
            ),
            
            div(class = "user-divider"),
            
            ## ---- Username as dropdown trigger ----
            div(
              class = "user-dropdown",
              
              div(
                class = "user-pill dropdown-trigger",
                display_user(),
                icon("chevron-down", class = "dropdown-caret")
              ),
              
              div(
                class = "user-dropdown-menu",
                
                actionLink(
                  inputId = "logout",
                  label = tagList(icon("sign-out-alt"), " Logout"),
                  class = "dropdown-logout-item"
                )
              )
            )
          )
        )
      ),
      
      ## =================================================================
      ## PAGE CONTENT
      ## =================================================================
      
      div(
        class = "main-page-container",
        page_body
      ),
      
      ## =================================================================
      ## TOAST CONTAINER
      ## =================================================================
      
      div(
        class = "toast-container bottom-right"
      )
    )
  })
}

## =============================================================================
## RUN APP
## =============================================================================

shinyApp(ui, server)


