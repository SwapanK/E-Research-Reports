# =============================================================================
# app.R  (main entry point)
# Loads libraries, sources helpers + ui/*.R + server/*.R by subtab, builds the
# dashboard shell (dashboard_style.css + Meta_styles.css, unmodified, same
# theme as the Vulnerability Sensitivity module), and runs the app.
# =============================================================================

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
# Helpers
# =============================================================================

source("R/helpers.R", local = TRUE)

# =============================================================================
# UI - one file per subtab
# =============================================================================

source("ui/home_ui.R", local = TRUE)
source("ui/preview_ui.R", local = TRUE)
source("ui/summary_ui.R", local = TRUE)
source("ui/profile_ui.R", local = TRUE)
source("ui/maps_ui.R", local = TRUE)
source("ui/ep_ui.R", local = TRUE)

# =============================================================================
# Server - one file per subtab
# =============================================================================

source("server/preview_server.R", local = TRUE)
source("server/summary_server.R", local = TRUE)
source("server/profile_server.R", local = TRUE)
source("server/maps_server.R", local = TRUE)
source("server/ep_server.R", local = TRUE)

# =============================================================================
# Default Packaged Data
# =============================================================================

def_aal <- load_aal("data/AAL_v13_byPerils_byLOB_byState_GReIED_USHU.csv")
def_ep <- load_ep("data/EP_v13_byPerils_byLOB_byState_GReIED_USHU.csv")

# =============================================================================
# HEADER  (same #6FACDE theme as the Vulnerability module - dashboard_style.css)
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
# SIDEBAR  (navigation only - Home + the single GReIED Analyzer page,
# matching the Vulnerability module's sidebar pattern)
# =============================================================================

app_sidebar <- dashboardSidebar(
  width = 260,
  sidebarMenu(
    id = "nav",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("GReIED Analyzer", tabName = "analyzer", icon = icon("chart-bar"))
  )
)

# =============================================================================
# ANALYZER PAGE
# One page, subtabs on the right (Data Preview / Executive Summary /
# State Profile / Geo Analytics / EP Analytics) - same app-shell / app-header /
# app-body / left-scroll-panel / right-panel pattern used by the
# Vulnerability Sensitivity module (Meta_styles.css, unmodified).
# =============================================================================

analyzer_ui <- function() {
  div(
    class = "app-shell",

    div(
      class = "app-header",
      div(
        class = "header-top",
        div(
          h1("GReIED Analyzer"),
          div(
            class = "header-sub",
            p("Explore AAL and EP output by peril, state, and line of business.")
          )
        )
      )
    ),

    div(
      class = "app-body",

      # ---- LEFT: shared filter panel (same idea as "1. Model Configuration") ----
      div(
        class = "left-scroll-panel",
        div(
          class = "sidebar-card",
          h4("Data controls"),
          fileInput("aal_file", "Optional AAL CSV", accept = ".csv"),
          fileInput("ep_file", "Optional EP CSV", accept = ".csv"),
          tags$hr(class = "soft"),
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
          checkboxInput("include_us", "Include US in state profile", TRUE)
        ),
        div(
          class = "sec2-hint",
          div(class = "sec2-hint-icon", icon("lightbulb")),
          div(
            "Charts show individual subperil values and normalised contribution ",
            "shares. PF_SU_TC remains available as a separate combined-peril ",
            "series in detailed charts and maps."
          )
        )
      ),

      # ---- RIGHT: description + subtabs (Processed Data / Regionwise / ...) ----
      div(
        class = "right-panel",
        div(
          class = "info-panel",
          h3("About this module"),
          p(
            "This module reviews GReIED Average Annual Loss (AAL) and ",
            "Exceedance Probability (EP) output across perils, states, and ",
            "lines of business."
          ),
          tags$ul(
            tags$li("Upload your own AAL / EP extracts or use the packaged GReIED dataset."),
            tags$li("Data Preview, Executive Summary, State Profile, Geo Analytics, and EP Analytics each use one consistent filter panel."),
            tags$li("Subperil selections and loss perspective apply across the AAL-driven subtabs and the Geo Analytics share map.")
          )
        ),
        tabsetPanel(
          id = "subtabs",
          tabPanel("Data Preview", preview_ui("preview")),
          tabPanel("Executive Summary", summary_ui("as")),
          tabPanel("State Profile", profile_ui("ap")),
          tabPanel("Geo Analytics", maps_ui("maps")),
          tabPanel("EP Analytics", ep_ui("ep"))
        )
      )
    )
  )
}

# =============================================================================
# BODY
# =============================================================================

app_body <- dashboardBody(
  tags$head(
    tags$title("GReIED Analyzer"),
    tags$link(rel = "stylesheet", href = "dashboard_style.css"),
    tags$link(rel = "stylesheet", href = "Meta_styles.css"),
    tags$link(rel = "stylesheet", href = "greied_extra.css"),
    tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),

  div(
    id = "app_ui",
    div(
      class = "container-fluid",
      tabItems(
        tabItem(tabName = "home", home_ui()),
        tabItem(tabName = "analyzer", analyzer_ui())
      )
    )
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
  summary_server("as", aal, af)
  profile_server("ap", aal, af)
  maps_server("maps", aal, ep, af)
  ep_server("ep", ep)

  # ---------------------------------------------------------------------
  # Home page navigation - jump to the analyzer page AND the right subtab
  # ---------------------------------------------------------------------
  observeEvent(input$go_preview, {
    updateTabItems(session, "nav", "analyzer")
    updateTabsetPanel(session, "subtabs", selected = "Data Preview")
  })
  observeEvent(input$go_summary, {
    updateTabItems(session, "nav", "analyzer")
    updateTabsetPanel(session, "subtabs", selected = "Executive Summary")
  })
  observeEvent(input$go_profile, {
    updateTabItems(session, "nav", "analyzer")
    updateTabsetPanel(session, "subtabs", selected = "State Profile")
  })
  observeEvent(input$go_maps, {
    updateTabItems(session, "nav", "analyzer")
    updateTabsetPanel(session, "subtabs", selected = "Geo Analytics")
  })
  observeEvent(input$go_ep, {
    updateTabItems(session, "nav", "analyzer")
    updateTabsetPanel(session, "subtabs", selected = "EP Analytics")
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
