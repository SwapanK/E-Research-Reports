# =============================================================================
# server/profile_server.R
# State Profile subtab server logic.
# =============================================================================

profile_server <- function(id, d, f) {
  moduleServer(id, function(input, output, session) {
    output$bar <- renderPlotly({

      x <- aal_long(d(), f()$state, f()$loss, f()$lob) %>% filter(Peril %in% f()$selected_perils)

      plot_ly(
        x,
        x = ~factor(Peril, levels = perils),
        y = ~Value,
        color = ~Peril,
        colors = cols,
        type = "bar"
      ) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(title = ""),
          yaxis = list(title = "AAL", tickprefix = "$")
        )
    })

    output$tab <- renderDT({
      x <- aal_long(d(), f()$state, f()$loss, f()$lob)
      p <- f()$selected_perils
      den <- sum(x$Value[x$Peril %in% p], na.rm = TRUE)

      x %>%
        mutate(Contribution = ifelse(Peril %in% p, Value / den, NA)) %>%
        select(Peril, AAL = Value, Contribution) %>%
        datatable(rownames = FALSE, options = list(dom = "t")) %>%
        formatCurrency("AAL", "$", 0) %>%
        formatPercentage("Contribution", 1)
    })

    output$gr <- renderPlotly({
      x <- d() %>%
        filter(State == f()$state) %>%
        select(Peril, LossPersp, all_of(f()$lob)) %>%
        rename(Value = all_of(f()$lob))

      plot_ly(
        x,
        x = ~factor(Peril, levels = perils),
        y = ~Value,
        color = ~LossPersp,
        type = "bar",
        colors = c(GU = "#1666A8", GR = "#F2A541")
      ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "AAL", tickprefix = "$")
        )
    })
  })
}
