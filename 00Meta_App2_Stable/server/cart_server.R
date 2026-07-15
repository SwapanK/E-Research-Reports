# =============================================================================
# CART SERVER - MODULE VERSION
# =============================================================================

plot_to_data_uri <- function(plot_obj, width = 8, height = 4.5, dpi = 120) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  uri <- base64enc::dataURI(file = tmp, mime = "image/png")
  unlink(tmp)
  uri
}

cart_server <- function(input, output, session, cart, username) {
  
  ## -------------------------------------------------------------------------
  ## SUMMARY TEXT
  ## -------------------------------------------------------------------------
  output$cart_summary_text <- renderUI({
    n <- length(cart())
    if (n == 0) "No items saved yet" else paste0(n, " item", if (n != 1) "s" else "", " saved")
  })
  
  ## -------------------------------------------------------------------------
  ## ITEM LIST
  ## -------------------------------------------------------------------------
  output$cart_items_ui <- renderUI({
    items <- cart()
    if (length(items) == 0) {
      return(
        div(
          class = "cart-empty-message",
          icon("shopping-cart"),
          p("Your cart is empty. Run an analysis and click 'Add To Cart'.")
        )
      )
    }
    tagList(
      lapply(seq_along(items), function(i) {
        item <- items[[i]]
        w <- item$width %||% 8
        h <- item$height %||% 4.5
        dpi <- item$dpi %||% 120
        div(
          class = "cart-item-card",
          div(
            class = "cart-item-header",
            div(
              span(class = "cart-item-badge", item$module %||% "Item"),
              tags$span(class = "cart-item-time", style = "margin-left:10px;",
                        format(item$timestamp, "%Y-%m-%d %H:%M"))
            ),
            tags$button(
              type    = "button",
              class   = "btn-remove-cart",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                session$ns("cart_remove_id"),   # <-- namespaced input ID
                item$id
              ),
              icon("trash"), " Remove"
            )
          ),
          if (!is.null(item$plot)) {
            tags$img(
              src   = plot_to_data_uri(item$plot, width = w, height = h, dpi = dpi),
              style = "width:100%; max-width:820px; display:block; margin:0 auto;"
            )
          },
          if (!is.null(item$commentary) && nzchar(item$commentary)) {
            div(class = "commentary-text", style = "margin-top:16px;", item$commentary)
          }
        )
      })
    )
  })
  
  ## -------------------------------------------------------------------------
  ## REMOVE ITEM
  ## -------------------------------------------------------------------------
  observeEvent(input$cart_remove_id, {
    id_to_remove <- input$cart_remove_id
    current_cart <- cart()
    current_cart <- Filter(function(x) !identical(x$id, id_to_remove), current_cart)
    cart(current_cart)
    
    session$sendCustomMessage(
      "show-toast",
      list(
        text     = "Item removed from cart",
        type     = "info",
        icon     = "fa-trash",
        duration = 2200
      )
    )
  })
  
  ## -------------------------------------------------------------------------
  ## GENERATE REPORT
  ## -------------------------------------------------------------------------
  output$generate_report <- downloadHandler(
    filename = function() {
      ext <- input$export_format %||% "pptx"
      paste0("Vulnerability_Report_", format(Sys.Date(), "%Y%m%d"), ".", ext)
    },
    content = function(file) {
      items <- cart()
      if (length(items) == 0) {
        showNotification("Cart is empty - nothing to export.", type = "error")
        return(NULL)
      }
      
      temp_cart <- tempfile(fileext = ".rds")
      saveRDS(items, temp_cart)
      
      fmt <- input$export_format %||% "pptx"
      tryCatch({
        switch(
          fmt,
          "pptx" = generate_cart_ppt(
            cart_path   = temp_cart,
            output_path = file,
            username    = username
          ),
          "docx" = generate_cart_docx(
            cart_path   = temp_cart,
            output_path = file,
            username    = username
          ),
          "html" = generate_cart_html(
            cart_path   = temp_cart,
            output_path = file,
            username    = username
          ),
          stop("Unknown format: ", fmt)
        )
        showNotification("Report generated.", type = "message")
      }, error = function(e) {
        showNotification(paste("Report generation failed:", e$message), type = "error")
      })
    }
  )
}

