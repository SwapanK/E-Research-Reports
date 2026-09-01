ep_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "panel-card",
      fluidRow(
        column(2, selectInput(ns("state"), "State", character())),
        column(2, selectInput(ns("loss"), "Loss perspective", character())),
        column(2, selectInput(ns("lob"), "LOB", character())),
        column(2, selectInput(ns("type"), "EP type", character())),
        column(2, selectInput(ns("rp"), "Return period", character())),
        column(
          2,
          checkboxGroupInput(
            ns("selected_perils"),
            "Subperils",
            choices = c("TC" = "TC", "SU" = "SU", "PF" = "PF", "IF" = "IF"),
            selected = c("TC", "SU", "PF")
          )
        )
      )
    ),

    uiOutput(ns("kpis")),

    fluidRow(
      column(
        5,
        div(
          class = "panel-card",
          h4("Subperil contribution at selected RP"),
          plotlyOutput(ns("pie"), height = "430px")
        )
      ),
      column(
        7,
        div(
          class = "panel-card",
          h4("EP curves by subperil"),
          plotlyOutput(ns("curves"), height = "430px")
        )
      )
    ),

    div(
      class = "panel-card",
      h4("State contribution profile at selected RP"),
      plotlyOutput(ns("states"), height = "900px")
    )
  )
}

ep_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    observeEvent(d(), {
      x <- d()

      updateSelectInput(
        session,
        "state",
        choices = c("US", setdiff(sort(unique(x$State)), "US")),
        selected = "US"
      )

      updateSelectInput(
        session,
        "loss",
        choices = sort(unique(x$LossPerspective))
      )

      updateSelectInput(
        session,
        "lob",
        choices = unique(x$LOB),
        selected = "Total"
      )

      updateSelectInput(
        session,
        "type",
        choices = unique(x$EPType)
      )

      updateSelectInput(
        session,
        "rp",
        choices = sort(unique(x$RP)),
        selected = 100
      )
    }, ignoreInit = FALSE)

    sel <- reactive({
      req(input$state, input$loss, input$lob, input$type, input$rp)

      ep_slice(
        d(),
        input$state,
        input$loss,
        input$lob,
        input$type,
        as.numeric(input$rp)
      )
    })

    output$kpis <- renderUI({
      x <- sel()

      v <- setNames(
        sapply(comps, function(p) {
          z <- x$Loss[x$Peril == p]
          if (length(z)) sum(z) else NA
        }),
        comps
      )

      req(input$selected_perils)
      req(length(input$selected_perils) > 0)

      ps <- input$selected_perils

      items <- list(
        c("Selected component loss", fmt(sum(v[ps], na.rm = TRUE)), "kpi-blue")
      )

      peril_kpis <- list(
        TC = c("TC", fmt(v["TC"]), "kpi-green"),
        SU = c("SU", fmt(v["SU"]), "kpi-orange"),
        PF = c("PF", fmt(v["PF"]), "kpi-yellow"),
        IF = c("IF", fmt(v["IF"]), "kpi-gold")
      )

      for (p in ps) {
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

    output$pie <- renderPlotly({
      x <- sel() %>%
        filter(Peril %in% input$selected_perils)

      validate(need(sum(x$Loss) > 0, "No data"))

      plot_ly(
        x,
        labels = ~Peril,
        values = ~Loss,
        type = "pie",
        hole = 0.52,
        textinfo = "label+percent",
        marker = list(
          colors = unname(cols[x$Peril]),
          line = list(color = "white", width = 2)
        )
      ) %>%
        layout(
          legend = list(orientation = "h", y = -0.05),
          margin = list(b = 55)
        )
    })

    output$curves <- renderPlotly({
      x <- d() %>%
        filter(
          State == input$state,
          LossPerspective == input$loss,
          LOB == input$lob,
          EPType == input$type,
          Peril %in% input$selected_perils
        )

      plot_ly(
        x,
        x = ~RP,
        y = ~Loss,
        color = ~Peril,
        colors = cols,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = "RP %{x}<br>%{fullData.name}: %{y:$,.4s}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Return period", type = "log"),
          yaxis = list(title = "Loss", tickprefix = "$"),
          legend = list(orientation = "h", y = -0.15),
          margin = list(b = 75)
        )
    })

    output$states <- renderPlotly({
      x <- ep_shares(
        d(),
        input$loss,
        input$lob,
        input$type,
        as.numeric(input$rp),
        input$selected_perils
      )

      states <- sort(unique(x$State))
      x$State <- factor(x$State, levels = rev(states))

      plot_ly(
        x,
        x = ~Share,
        y = ~State,
        color = ~Peril,
        colors = cols,
        type = "bar",
        orientation = "h",
        customdata = ~Loss,
        hovertemplate = paste0(
          "%{y}<br>%{fullData.name}: %{x:.1%}",
          "<br>%{customdata:$,.4s}<extra></extra>"
        )
      ) %>%
        layout(
          barmode = "stack",
          xaxis = list(title = "Share", tickformat = ".0%"),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(states),
            dtick = 1,
            automargin = TRUE
          ),
          legend = list(orientation = "h", y = -0.03),
          margin = list(l = 60, b = 60)
        )
    })
  })
}
