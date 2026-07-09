library(shiny)
library(shinyjs)
library(ggplot2)

## =============================================================================
## SOURCE MODULE FILES  (shared plotting, cart persistence, PPT export)
## =============================================================================

source("module/cart_utils.R")
source("module/plotting_function.R")
source("module/ppt_generator.R")
source("module/docx_generator.R")   # <-- NEW: generate_cart_docx()
source("module/rmd_generator.R")    # <-- NEW: generate_cart_html()

## =============================================================================
## SOURCE UI FILES
## =============================================================================

source("ui/login_ui.R")
source("ui/dashboard_ui.R")
source("ui/vulnerability_ui.R")
source("ui/secmod_ui.R")
source("ui/cart_ui.R")

## =============================================================================
## SOURCE SERVER FILES
## =============================================================================

source("server/login_server.R")
source("server/dashboard_server.R")
source("server/vulnerability_server.R")
source("server/secmod_server.R")
source("server/cart_server.R")

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
  ## CART — persisted per-user under cart/<username>_cart.rds
  ## =========================================================================

  cart <- reactiveVal(
    load_cart(isolate(display_user()))
  )

  ## =========================================================================
  ## LOGIN EVENT
  ## =========================================================================

  observeEvent(input$enter_app, {
    logged_in(TRUE)
    active_page("dashboard")

    # Reload this user's saved cart in case it changed outside this session
    cart(load_cart(display_user()))
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

  observeEvent(input$goto_vulnerability, {
    active_page("vulnerability")
  })

  observeEvent(input$goto_secmod, {
    active_page("secmod")
  })

  ## =========================================================================
  ## MODULE SERVERS
  ## =========================================================================

  login_server(input, output, session, logged_in)

  dashboard_server(input, output, session, username = display_user, cart = cart)

  vulnerability_server(input, output, session, cart = cart, username = display_user)

  secmod_server(input, output, session, cart = cart, username = display_user)

  cart_server(input, output, session, cart = cart, username = display_user)

  ## =========================================================================
  ## CART BADGE (navbar icon)
  ## =========================================================================

  output$cart_count <- renderUI({
    span(
      class = "cart-side-badge",
      length(cart())
    )
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
