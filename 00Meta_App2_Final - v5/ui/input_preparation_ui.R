# UI module for Input File Creation App

# Helper: lightbulb hint box
sec_hint <- function(text) {
  div(
    class = "sec2-hint",
    span(class = "sec2-hint-icon", icon("lightbulb")),
    span(text)
  )
}

inputPrepUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      class = "app-shell",
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Input File Creation App for Vulnerability Sensitivity and Secondary Modifier Modules")
          # Logo removed from header
        ),
        div(
          class = "header-sub",
          p("Create, review and download standardized model input files for RMS and Verisk platforms")
        )
      ),
      div(
        class = "app-body",
        div(
          class = "left-scroll-panel",
          div(
            class = "sidebar-card",
            h4("1. Model Selection"),
            sec_hint("Select vendor, module, and model version."),
            selectInput(ns("vendor"), "Vendor model family",
                        choices = c("Moody's", "Verisk"),
                        selected = "Moody's",
                        width = "100%", selectize = FALSE),
            uiOutput(ns("version_ui")),
            selectInput(ns("module"), "Module",
                        choices = c("Vulnerability Sensitivity", "Secondary Modifiers"),
                        selected = "Vulnerability Sensitivity",
                        width = "100%", selectize = FALSE),
            tags$hr(class = "soft"),
            h4("2. Exposure Settings"),
            sec_hint("Define country, peril, subperil, and exposure values."),
            textInput(ns("country"), "Country code", value = "US", width = "100%"),
            selectInput(ns("peril"), "Peril",
                        choices = names(peril_lookup()),
                        selected = "SCS",
                        width = "100%", selectize = FALSE),
            uiOutput(ns("subperil_ui")),
            textInput(ns("suffix"), "Suffix", value = "2026", width = "100%"),
            numericInput(ns("bldgVal"), "Building value", value = 1000000, min = 0, width = "100%"),
            numericInput(ns("cntVal"), "Contents value", value = 500000, min = 0, width = "100%"),
            numericInput(ns("BIVal"), "BI value", value = 100000, min = 0, width = "100%"),
            tags$hr(class = "soft"),
            h4("3. Upload / Sample Input Files"),
            sec_hint("Upload a Locations file (.csv) and a Combinations file (.csv) matching your selected vendor and module. Alternatively, click 'Use included sample files' below to preview the format with bundled data."),
            fileInput(ns("location_file"), "Locations File CSV", accept = ".csv", width = "100%"),
            fileInput(ns("combination_file"), "Combinations File CSV", accept = ".csv", width = "100%"),
            actionButton(ns("use_samples"), "Use included sample files", class = "primary-btn"),
            div(class = "small-note", textOutput(ns("sample_note"))),
            div(
              class = "sample-row",
              downloadButton(ns("download_sample_location"), "Sample Locations"),
              downloadButton(ns("download_sample_combination"), "Sample Combinations")
            ),
            tags$hr(class = "soft"),
            actionButton(ns("create_files"), "Create Input Files", class = "create-btn")
          )
        ),
        div(
          class = "right-panel",
          uiOutput(ns("status_ui")),
          div(
            class = "info-panel",
            h3("About this section"),
            p("This application provides a streamlined workflow for generating Vulnerability Sensitivity (Vulsens) and Secondary Modifier (Secmod) test input files for Moody's RMS and Verisk catastrophe modelling platforms."),
            p("Follow the steps below to create model‑ready testing files:"),
            tags$ol(
              tags$li("Select the modelling platform and module type."),
              tags$li("Define model configuration and exposure parameters."),
              tags$li("Upload location and combination datasets or use the provided sample files."),
              tags$li("Review and validate inputs through the interactive preview dashboard."),
              tags$li("Generate standardized Location and Account input files."),
              tags$li("Review generated outputs prior to deployment."),
              tags$li("Download the final files for testing and model evaluation workflows.")
            )
          ),
          # ---- Progress stepper placeholder ----
          uiOutput(ns("progress_panel")),
          
          tabsetPanel(
            tabPanel("Input Dashboard", br(), uiOutput(ns("input_dashboard_ui"))),
            tabPanel("Output Dashboard", br(), uiOutput(ns("output_dashboard_ui")))
          )
        )
      )
    )
  )
}

# Helper UI components (used only in this module)
input_preview_table <- function(id, title) {
  div(class = "preview-card",
      h4(title),
      DTOutput(id))
}

output_preview_table <- function(id, title, download_id = NULL, label = NULL) {
  div(class = "preview-card",
      div(class = "preview-toolbar",
          h4(title),
          if (!is.null(download_id)) downloadButton(download_id, label %||% "Download")
      ),
      DTOutput(id))
}

`%||%` <- function(a, b) if (is.null(a)) b else a











