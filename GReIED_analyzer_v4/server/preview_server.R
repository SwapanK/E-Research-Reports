# =============================================================================
# server/preview_server.R
# Data Preview subtab server logic.
# =============================================================================

preview_server <- function(id, aal_r, ep_r) {
  moduleServer(id, function(input, output, session) {

    # -------------------------------------------------------------------------
    # Metadata
    # -------------------------------------------------------------------------
    output$aal_meta <- renderText({
      x <- aal_r()
      paste(
        format(nrow(x), big.mark = ","),
        "rows |",
        ncol(x),
        "columns"
      )
    })

    output$ep_meta <- renderText({
      x <- ep_r()
      paste(
        format(nrow(x), big.mark = ","),
        "rows |",
        ncol(x),
        "columns"
      )
    })

    # -------------------------------------------------------------------------
    # AAL Dataset
    # -------------------------------------------------------------------------
    output$aal_table <- renderDT({
      datatable(
        aal_r(),
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 15,
          lengthMenu = c(15, 25, 50, 100),
          scrollX = TRUE,
          scrollY = "700px",
          deferRender = TRUE
        )
      ) %>%
        formatCurrency(
          c("Commercial", "Personal", "Total"),
          currency = "$",
          digits = 0
        )
    })

    # -------------------------------------------------------------------------
    # EP Dataset
    # -------------------------------------------------------------------------
    output$ep_table <- renderDT({
      datatable(
        ep_r(),
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 15,
          lengthMenu = c(15, 25, 50, 100),
          scrollX = TRUE,
          scrollY = "700px",
          deferRender = TRUE
        )
      ) %>%
        formatCurrency("Loss", currency = "$", digits = 0)
    })

  })
}
