# =============================================================================
# CART SERVER
# =============================================================================
# cart      : shared reactiveVal(list()) from App.R
# username  : reactive() returning the current display username
# =============================================================================

## -----------------------------------------------------------------------
## Render a ggplot object to a base64 PNG data URI so it can be embedded
## directly as a static <img>, one per cart item, without needing a
## separately-registered Shiny output for each dynamically created item.
## -----------------------------------------------------------------------
plot_to_data_uri <- function(plot_obj, width = 8, height = 4.5, dpi = 120) {

  tmp <- tempfile(fileext = ".png")

  ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")

  uri <- base64enc::dataURI(file = tmp, mime = "image/png")

  unlink(tmp)

  uri
}

cart_server <- function(input, output, session, cart, username) {

  ## =========================================================================
  ## SUMMARY TEXT
  ## =========================================================================

  output$cart_summary_text <- renderUI({
    n <- length(cart())
    if (n == 0) {
      "No items saved yet"
    } else {
      paste0(n, " item", if (n != 1) "s" else "", " saved")
    }
  })

  ## =========================================================================
  ## ITEM LIST — plot then commentary, one item after another
  ## =========================================================================

  output$cart_items_ui <- renderUI({

    items <- cart()

    if (length(items) == 0) {
      return(
        div(
          class = "cart-empty-message",
          icon("shopping-cart"),
          p("Your cart is empty. Run an analysis on the Vulnerability or Secondary Modifier page and click \"Add To Cart\".")
        )
      )
    }

    tagList(
      lapply(seq_along(items), function(i) {

        item <- items[[i]]

        div(
          class = "cart-item-card",

          div(
            class = "cart-item-header",

            div(
              span(class = "cart-item-badge", item$module %||% "Item"),
              tags$span(
                class = "cart-item-time",
                style = "margin-left:10px;",
                format(item$timestamp, "%Y-%m-%d %H:%M")
              )
            ),

            tags$button(
              type    = "button",
              class   = "btn-remove-cart",
              onclick = sprintf(
                "Shiny.setInputValue('cart_remove_id', '%s', {priority: 'event'})",
                item$id
              ),
              icon("trash"), " Remove"
            )
          ),

          if (!is.null(item$plot)) {
            tags$img(
              src   = plot_to_data_uri(item$plot),
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

  ## =========================================================================
  ## REMOVE ITEM
  ## =========================================================================

  observeEvent(input$cart_remove_id, {

    id_to_remove <- input$cart_remove_id

    current_cart <- cart()
    current_cart <- Filter(function(x) !identical(x$id, id_to_remove), current_cart)
    cart(current_cart)

    save_cart(username(), current_cart)

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

  ## =========================================================================
  ## GENERATE REPORT — works from cart/<username>_cart.rds, in whichever
  ## format the user picked with the "export_format" selectInput (PPT /
  ## DOCX / HTML). Each generator lives in its own module file:
  ##   - module/ppt_generator.R  -> generate_cart_ppt()
  ##   - module/docx_generator.R -> generate_cart_docx()
  ##   - module/rmd_generator.R  -> generate_cart_html()
  ## =========================================================================

  output$generate_report <- downloadHandler(

    filename = function() {
      ext <- input$export_format %||% "pptx"
      paste0("VulSen_Report_", username(), "_", format(Sys.Date(), "%Y%m%d"), ".", ext)
    },

    content = function(file) {

      items <- cart()

      if (length(items) == 0) {
        showNotification("Cart is empty — nothing to export.", type = "error")
        return(NULL)
      }

      fmt <- input$export_format %||% "pptx"

      tryCatch({
        switch(
          fmt,
          "pptx" = generate_cart_ppt(
            cart_path   = get_cart_path(username()),
            output_path = file,
            username    = username()
          ),
          "docx" = generate_cart_docx(
            cart_path   = get_cart_path(username()),
            output_path = file,
            username    = username()
          ),
          "html" = generate_cart_html(
            cart_path   = get_cart_path(username()),
            output_path = file,
            username    = username()
          ),
          stop("Unknown export format: ", fmt)
        )

        showNotification(
          paste0("Report generated (", toupper(fmt), ")."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Report generation failed:", conditionMessage(e)), type = "error")
      })
    }
  )
}
