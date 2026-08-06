# server/cart_server.R

# Serve a temp directory for generated pptx/docx/html reports so the browser
# can download them via a JS-triggered link click (registered once at source time).
.cart_download_dir <- file.path(tempdir(), "cart_download")
dir.create(.cart_download_dir, showWarnings = FALSE, recursive = TRUE)
addResourcePath("cart_download", .cart_download_dir)

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
  ## REMOVE ALL
  ## -------------------------------------------------------------------------
  observeEvent(input$cart_remove_all, {
    showModal(
      modalDialog(
        title = "Remove all items?",
        "This will remove all items from your cart. Are you sure?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_remove_all"), "Yes, remove all", class = "btn-danger")
        ),
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$confirm_remove_all, {
    removeModal()
    cart(list())  # clear reactive cart
    
    # Also persist empty cart to disk (if needed)
    save_cart(username, list())
    
    session$sendCustomMessage(
      "show-toast",
      list(
        text     = "All items removed from cart",
        type     = "info",
        icon     = "fa-trash",
        duration = 2200
      )
    )
  })
  
  ## -------------------------------------------------------------------------
  ## GENERATE REPORT (pptx / docx / html)
  ## -------------------------------------------------------------------------
  rv_report <- reactiveValues(
    generating     = FALSE,
    last_signature = NULL
  )
  
  format_label <- function(fmt) {
    switch(fmt, "pptx" = "PowerPoint", "docx" = "Word", "html" = "HTML", fmt)
  }
  
  run_report_generation <- function() {
    items <- cart()
    if (length(items) == 0) {
      showNotification("Cart is empty - nothing to export.", type = "error")
      return()
    }
    
    fmt <- input$export_format %||% "pptx"
    rv_report$generating <- TRUE
    
    notif_id <- showNotification(
      paste0("Generating ", format_label(fmt), " report... please wait"),
      duration    = NULL,
      closeButton = FALSE,
      type        = "message"
    )
    
    temp_cart <- tempfile(fileext = ".rds")
    saveRDS(items, temp_cart)
    
    fname <- paste0("Vulnerability_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
    out_path <- file.path(.cart_download_dir, fname)
    
    tryCatch({
      switch(
        fmt,
        "pptx" = generate_cart_ppt(cart_path = temp_cart, output_path = out_path, username = username),
        "docx" = generate_cart_docx(cart_path = temp_cart, output_path = out_path, username = username),
        "html" = generate_cart_html(cart_path = temp_cart, output_path = out_path, username = username),
        stop("Unknown format: ", fmt)
      )
      
      removeNotification(notif_id)
      showNotification(paste0(format_label(fmt), " report generated."), type = "message", duration = 4)
      
      session$sendCustomMessage("cart-trigger-download", list(
        url      = file.path("cart_download", fname),
        filename = fname
      ))
      
      rv_report$last_signature <- paste0(fmt, "|", paste(vapply(items, function(x) x$id, character(1)), collapse = ","))
    }, error = function(e) {
      removeNotification(notif_id)
      showNotification(paste("Report generation failed:", e$message), type = "error", duration = NULL)
    })
    
    rv_report$generating <- FALSE
  }
  
  observeEvent(input$generate_report_btn, {
    items <- cart()
    if (length(items) == 0) {
      showNotification("Cart is empty - nothing to export.", type = "error")
      return()
    }
    if (isTRUE(rv_report$generating)) {
      showNotification("Still generating the previous report - please wait.", type = "warning")
      return()
    }
    
    fmt <- input$export_format %||% "pptx"
    sig <- paste0(fmt, "|", paste(vapply(items, function(x) x$id, character(1)), collapse = ","))
    
    if (!is.null(rv_report$last_signature) && identical(sig, rv_report$last_signature)) {
      showModal(
        modalDialog(
          title = "Report already generated",
          paste0("A ", format_label(fmt), " report was already generated for these items. Regenerate it?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_regenerate_report"), "Yes, regenerate", class = "btn-glass")
          ),
          easyClose = FALSE
        )
      )
      return()
    }
    
    run_report_generation()
  })
  
  observeEvent(input$confirm_regenerate_report, {
    removeModal()
    run_report_generation()
  })
}



