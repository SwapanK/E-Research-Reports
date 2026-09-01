# =============================================================================
# server/summary_server.R
# Executive Summary subtab server logic.
# =============================================================================

summary_server <- function(id, d, f) {
  moduleServer(id, function(input, output, session) {
    output$kpis <- renderUI({
      x <- aal_long(d(), f()$state, f()$loss, f()$lob)
      v <- vals(x)

      items <- list(
        c("Selected component AAL",
          fmt(sum(v[f()$selected_perils], na.rm = TRUE)),
          "kpi-blue"
        )
      )

      peril_kpis <- list(
        TC = c("TC", fmt(v["TC"]), "kpi-green"),
        SU = c("SU", fmt(v["SU"]), "kpi-orange"),
        PF = c("PF", fmt(v["PF"]), "kpi-yellow"),
        IF = c("IF", fmt(v["IF"]), "kpi-gold")
      )

      for (p in f()$selected_perils) {
        items <- append(items, list(peril_kpis[[p]]))
      }

      w <- paste0(round(100 / length(items), 1), "%")

      div(
        class = "kpi-row",
        lapply(items, function(a) {
          div(
            style = paste0("width:", w),
            div(
              class = paste("kpi", a[3]),
              div(class = "kpi-label", a[1]),
              div(class = "kpi-value", a[2])
            )
          )
        })
      )
    })

    output$donut <- renderPlotly({
      x <- aal_long(d(), f()$state, f()$loss, f()$lob) %>%
        filter(
          Peril %in% f()$selected_perils
        )

      validate(need(sum(x$Value, na.rm = TRUE) > 0, "No data"))

      plot_ly(
        x,
        labels = ~Peril,
        values = ~Value,
        type = "pie",
        hole = 0.52,
        textinfo = "label+percent",
        marker = list(
          colors = unname(cols[x$Peril]),
          line = list(color = "white", width = 2)
        ),
        hovertemplate = "%{label}<br>%{value:$,.4s}<br>%{percent}<extra></extra>"
      ) %>%
        layout(
          legend = list(orientation = "h", y = -0.05),
          margin = list(l = 5, r = 5, t = 5, b = 55),
          annotations = list(
            list(
              text = paste(f()$state, f()$loss, f()$lob, sep = "<br>"),
              showarrow = FALSE
            )
          )
        )
    })

    output$stack <- renderPlotly({
      x <- d() %>%
        filter(LossPersp == f()$loss, Peril %in% f()$selected_perils) %>%
        select(State, Peril, all_of(f()$lob)) %>%
        rename(Value = all_of(f()$lob))

      if (!f()$include_us) {
        x <- filter(x, State != "US")
      }

      x <- x %>%
        group_by(State) %>%
        mutate(
          TotalValue = sum(Value, na.rm = TRUE),
          Share = if_else(TotalValue > 0, Value / TotalValue, 0)
        ) %>%
        ungroup()

      ord <- x %>%
        group_by(State) %>%
        summarise(z = sum(Value), .groups = "drop") %>%
        arrange(z)

      x$State <- factor(x$State, levels = ord$State)

      plot_ly(
        x,
        x = ~Share,
        y = ~State,
        color = ~Peril,
        colors = cols,
        type = "bar",
        orientation = "h",
        customdata = ~Value,
        hovertemplate = "%{y}<br>%{fullData.name}: %{x:.1%}<br>%{customdata:$,.4s}<extra></extra>"
      ) %>%
        layout(
          barmode = "stack",
          xaxis = list(
            title = "Share",
            tickformat = ".0%"
          ),
          yaxis = list(
            title = "",
            automargin = TRUE,
            categoryorder = "array",
            categoryarray = ord$State
          ),
          legend = list(
            orientation = "h",
            y = -0.04
          ),
          margin = list(
            l = 55,
            r = 10,
            t = 5,
            b = 60
          )
        )
    })

  })
}
