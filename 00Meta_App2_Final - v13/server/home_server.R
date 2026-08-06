# =============================================================================
# HOME SERVER MODULE
# Emits a navigation target (reactiveVal) that the top-level server observes
# to switch active_page(). Also renders the live cart count in the hero.
# =============================================================================

homeServer <- function(input, output, session, cart = NULL) {

  nav_target <- reactiveVal(NULL)
  go <- function(page) nav_target(page)

  # ---- Hero buttons ----
  observeEvent(input$go_prep_hero,  go("input_prep"))
  observeEvent(input$go_cart_hero,  go("cart"))

  # ---- Workflow pipeline steps ----
  observeEvent(input$go_flow_prep,    go("input_prep"))
  observeEvent(input$go_flow_extract, go("result_extract"))
  observeEvent(input$go_flow_secmod,  go("secmod"))
  observeEvent(input$go_flow_cart,    go("cart"))

  # ---- Feature cards ----
  observeEvent(input$go_prep_card,    go("input_prep"))
  observeEvent(input$go_extract_card, go("result_extract"))
  observeEvent(input$go_vulsen_card,  go("vulsen"))
  observeEvent(input$go_secmod_card,  go("secmod"))
  observeEvent(input$go_cart_card,    go("cart"))

  # ---- Live cart count in hero stats + button badge ----
  output$hero_cart_count <- renderUI({
    n <- if (!is.null(cart)) length(cart()) else 0
    div(class = "home-stat-num", n)
  })

  output$hero_cart_badge <- renderUI({
    n <- if (!is.null(cart)) length(cart()) else 0
    if (n > 0) span(class = "home-cart-badge", n) else NULL
  })

  # Return the reactiveVal so the top-level server can watch it
  nav_target
}
