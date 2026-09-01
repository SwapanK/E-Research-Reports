options(shiny.maxRequestSize = 150 * 1024^2)

pkgs <- c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr", "readr", "scales", "DT")

miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(miss)) {
  stop(
    "Install packages: install.packages(c(",
    paste(sprintf('"%s"', miss), collapse = ", "),
    "))"
  )
}

lapply(pkgs, library, character.only = TRUE)

# =============================================================================
# Load Modules
# =============================================================================

source("R/helpers.R", local = TRUE)
source("R/mod_home.R", local = TRUE)
source("R/mod_preview.R", local = TRUE)
source("R/mod_aal.R", local = TRUE)
source("R/mod_ep.R", local = TRUE)
source("R/mod_maps.R", local = TRUE)

# =============================================================================
# Default Packaged Data
# =============================================================================

def_aal <- load_aal("data/AAL_v13_byPerils_byLOB_byState_GReIED_USHU.csv")
def_ep <- load_ep("data/EP_v13_byPerils_byLOB_byState_GReIED_USHU.csv")

# =============================================================================
# Small helper: persistent "app description" banner shown at the top of
# every analytics page (title + one-line description of that page).
# =============================================================================

page_head <- function(title, desc) {
  div(
    class = "page-head",
    h3(title),
    p(desc)
  )
}

# =============================================================================
# HEADER  (navy top bar, custom logo + app name, no default shinydashboard box)
# =============================================================================

app_header <- dashboardHeader(
  title = NULL,
  tags$li(
    class = "dropdown header-left-item",
    div(
      class = "header-left",
      tags$img(src = "logo.png", class = "header-logo-img",
               onerror = "this.style.display='none'"),
      span(class = "header-app-name", "GReIED Analyzer")
    )
  ),
  tags$li(
    class = "dropdown header-right-item",
    div(
      class = "header-right",
      span(class = "header-tagline",
           "Property Research | Catastrophe Model Attribution and Diagnostics")
    )
  )
)

# =============================================================================
# SIDEBAR  (navy, navigation menu + shared data/AAL filter controls)
# =============================================================================

app_sidebar <- dashboardSidebar(
  width = 300,

  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data Preview", tabName = "preview", icon = icon("table")),
    menuItem("Executive Summary", tabName = "summary", icon = icon("chart-pie")),
    menuItem("State Profile", tabName = "profile", icon = icon("chart-bar")),
    menuItem("Geo Analytics", tabName = "maps", icon = icon("globe")),
    menuItem("EP Analytics", tabName = "ep", icon = icon("chart-line"))
  ),

  div(
    class = "filter-card sidebar-filter-card",
    h4("Data controls"),
    fileInput("aal_file", "Optional AAL CSV", accept = ".csv"),
    fileInput("ep_file", "Optional EP CSV", accept = ".csv"),
    hr(class = "soft"),
    h4("AAL controls"),
    selectInput("aal_state", "State", character()),
    radioButtons(
      "aal_loss", "Loss perspective",
      c("Ground Up (GU)" = "GU", "Gross (GR)" = "GR"),
      selected = "GU", inline = TRUE
    ),
    radioButtons(
      "aal_lob", "Line of business",
      c("Total", "Commercial", "Personal"),
      selected = "Total"
    ),
    checkboxGroupInput(
      "selected_perils",
      "Subperils",
      choices = c("TC" = "TC", "SU" = "SU", "PF" = "PF", "IF" = "IF"),
      selected = c("TC", "SU", "PF")
    ),
    checkboxInput("include_us", "Include US in state profile", TRUE),
    hr(class = "soft"),
    div(
      class = "method-note",
      strong("Scientific use"),
      p(
        "Charts show individual subperil values and normalised contribution shares. ",
        "PF_SU_TC remains available as a separate combined-peril series in detailed ",
        "charts and maps."
      )
    )
  )
)

# =============================================================================
# BODY
# =============================================================================

app_body <- dashboardBody(
  tags$head(
    tags$title("GReIED Analyzer"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),

  tabItems(
    tabItem(
      tabName = "home",
      home_ui()
    ),

    tabItem(
      tabName = "preview",
      page_head("Data Preview", "Loaded AAL and EP datasets, filterable and exportable."),
      preview_ui("preview")
    ),

    tabItem(
      tabName = "summary",
      page_head("Executive Summary", "KPI tiles, contribution donut, and state contribution profile for the selected perils."),
      aal_summary_ui("as")
    ),

    tabItem(
      tabName = "profile",
      page_head("State Profile", "AAL by peril for the selected state, contribution table, and GU vs GR comparison."),
      aal_profile_ui("ap")
    ),

    tabItem(
      tabName = "maps",
      page_head("Geo Analytics", "Choropleth maps of value and subperil contribution share by state."),
      maps_ui("maps")
    ),

    tabItem(
      tabName = "ep",
      page_head("EP Analytics", "Exceedance probability curves, subperil pies, and state contribution profiles by return period."),
      ep_ui("ep")
    )
  ),

  div(
    class = "app-footer",
    "GReIED Analyzer | Internal analytical tool | Human review required before stakeholder use"
  )
)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  title = "GReIED Analyzer",
  skin = "blue",
  app_header,
  app_sidebar,
  app_body
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  aal <- reactive({
    if (is.null(input$aal_file)) def_aal else load_aal(input$aal_file$datapath)
  })

  ep <- reactive({
    if (is.null(input$ep_file)) def_ep else load_ep(input$ep_file$datapath)
  })

  observeEvent(
    aal(),
    {
      s <- sort(unique(aal()$State))

      updateSelectInput(
        session,
        "aal_state",
        choices = c("US", setdiff(s, "US")),
        selected = if ("US" %in% s) "US" else s[1]
      )
    },
    ignoreInit = FALSE
  )

  af <- reactive({
    req(input$aal_state)
    req(input$selected_perils)
    req(length(input$selected_perils) > 0)

    list(
      state = input$aal_state,
      loss = input$aal_loss,
      lob = input$aal_lob,
      selected_perils = input$selected_perils,
      include_us = isTRUE(input$include_us)
    )
  })

  preview_server("preview", aal, ep)
  aal_summary_server("as", aal, af)
  aal_profile_server("ap", aal, af)
  maps_server("maps", aal, ep, af)
  ep_server("ep", ep)

  # ---------------------------------------------------------------------
  # Home page navigation (hero buttons + feature cards)
  # ---------------------------------------------------------------------
  observeEvent(input$go_preview, updateTabItems(session, "tabs", "preview"))
  observeEvent(input$go_summary, updateTabItems(session, "tabs", "summary"))
  observeEvent(input$go_profile, updateTabItems(session, "tabs", "profile"))
  observeEvent(input$go_maps, updateTabItems(session, "tabs", "maps"))
  observeEvent(input$go_ep, updateTabItems(session, "tabs", "ep"))
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
