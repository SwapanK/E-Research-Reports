# =============================================================================
# server/maps_server.R
# Geo Analytics subtab server logic.
# =============================================================================

maps_server <- function(id, aal, ep, f) {


  moduleServer(id, function(input, output, session) {
    observe({
      available_perils <- sort(unique(if (input$source == "AAL") aal()$Peril else ep()$Peril))

      updateSelectInput(
        session,
        "metric",
        choices = available_perils,
        selected = available_perils[1]
      )
    })

    # -------------------------------------------------------------------------
    # Populate EP dropdowns
    # -------------------------------------------------------------------------
    observeEvent(ep(), {
      updateSelectInput(
        session,
        "type",
        choices = unique(ep()$EPType)
      )

      updateSelectInput(
        session,
        "rp",
        choices = sort(unique(ep()$RP)),
        selected = 100
      )
    }, ignoreInit = FALSE)

    # -------------------------------------------------------------------------
    # Loss perspective changes
    # -------------------------------------------------------------------------
    observe({
      choices <- if (input$source == "AAL") {
        sort(unique(aal()$LossPersp))
      } else {
        sort(unique(ep()$LossPerspective))
      }

      updateSelectInput(
        session,
        "loss",
        choices = choices,
        selected = choices[1]
      )
    })

    # -------------------------------------------------------------------------
    # Value map data
    # -------------------------------------------------------------------------
    md <- reactive({
      req(input$source, input$metric, input$loss, input$lob)

      if (input$source == "AAL") {
        x <- aal() %>%
          filter(State != "US", LossPersp == input$loss) %>%
          select(State, Peril, all_of(input$lob)) %>%
          rename(Value = all_of(input$lob))

      } else {
        req(input$type, input$rp)

        x <- ep() %>%
          filter(
            State != "US",
            LossPerspective == input$loss,
            LOB == input$lob,
            EPType == input$type,
            RP == as.numeric(input$rp)
          ) %>%
          transmute(State, Peril, Value = Loss)
      }

      x %>%
        filter(
          Peril == input$metric
        )
    })

    # -------------------------------------------------------------------------
    # Share map data
    # -------------------------------------------------------------------------
    sd <- reactive({
      req(input$source, input$metric, input$loss, input$lob)

      p <- input$metric
      if (!(p %in% f()$selected_perils)) {

        return(
          data.frame(
            State = character(),
            Share = numeric()
          )
        )

      }

      if (input$source == "AAL") {
        shares_aal(aal(), input$loss, input$lob, f()$selected_perils, FALSE) %>%
          filter(Peril == p) %>%
          select(State, Share)

      } else {
        req(input$type, input$rp)

        ep_shares(
          ep(),
          input$loss, input$lob,
          input$type, as.numeric(input$rp), f()$selected_perils) %>%
          filter(State != "US", Peril == p) %>%
          select(State, Share)
      }
    })

    # -------------------------------------------------------------------------
    # Value map
    # -------------------------------------------------------------------------
    output$value <- renderPlotly({
      x <- md()

      z_value <- if (isTRUE(input$log_scale)) {
        log10(pmax(x$Value, 1))
      } else {
        x$Value
      }

      scale_title <- if (isTRUE(input$log_scale)) {
        "log10(Value)"
      } else {
        "Value"
      }

      plot_geo(x, locationmode = "USA-states") %>%
        add_trace(
          type = "choropleth",
          locations = ~State,
          z = z_value,
          text = ~paste0(State, "<br>", scales::dollar(Value)),
          hoverinfo = "text",
          colorscale = "Blues",
          marker = list(
            line = list(
              color = "white",
              width = 0.8
            )
          ),
          colorbar = list(title = scale_title)
        ) %>%
        layout(
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa")
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0)
        )
    })

    # -------------------------------------------------------------------------
    # Share map
    # -------------------------------------------------------------------------
    output$share <- renderPlotly({
      x <- sd()

      plot_geo(x, locationmode = "USA-states") %>%
        add_trace(
          type = "choropleth",
          locations = ~State,
          z = ~Share,
          text = ~paste0(State, "<br>", scales::percent(Share, accuracy = 0.1)),
          hoverinfo = "text",
          colorscale = "Viridis",
          zmin = 0,
          zmax = 1,
          marker = list(
            line = list(
              color = "white",
              width = 0.8
            )
          ),
          colorbar = list(
            title = "Share",
            tickformat = ".0%"
          )
        ) %>%
        layout(
          geo = list(
            scope = "usa",
            projection = list(type = "albers usa")
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0)
        )
    })
  })
}
