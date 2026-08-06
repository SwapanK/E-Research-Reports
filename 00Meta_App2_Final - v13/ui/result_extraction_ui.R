# UI module for Result Extraction App

# Helper: lightbulb hint box
sec_hint <- function(text) {
  div(
    class = "sec2-hint",
    span(class = "sec2-hint-icon", icon("lightbulb")),
    span(text)
  )
}

resultExtractUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      class = "app-shell",
      div(
        class = "app-header",
        div(
          class = "header-top",
          h1("Result Extraction App for Vulnerability Sensitivity and Secondary Modifier Modules")
        ),
        div(
          class = "header-sub",
          p("Extract, review and download standardized model outputs from RMS and Verisk environments")
        )
      ),
      div(
        class = "app-body",
        div(
          class = "left-scroll-panel",
          div(
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("1. Model Selection"),
              uiOutput(ns("extract_settings_status"), inline = TRUE)
            ),
            sec_hint("Select vendor, module, and number of versions."),
            selectInput(ns("vendor"), "Vendor model family",
                        choices = c("Moody's", "Verisk"),
                        width = "100%", selectize = FALSE),
            selectInput(ns("module"), "Module",
                        choices = c("Vulnerability Sensitivity", "Secondary Modifiers"),
                        width = "100%", selectize = FALSE),
            uiOutput(ns("version_count_ui")),
            tags$hr(class = "soft")
          ),
          div(
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("2. Exposure Settings"),
              uiOutput(ns("extract_exposure_status"), inline = TRUE)
            ),
            sec_hint("Define country, peril, subperil, and suffix."),
            textInput(ns("country"), "Country code", value = "US", width = "100%"),
            selectInput(ns("peril"), "Peril",
                        choices = names(peril_lookup()),
                        selected = "SCS",
                        width = "100%", selectize = FALSE),
            uiOutput(ns("subperil_ui")),
            textInput(ns("suffix"), "Suffix", value = "2026", width = "100%")
          ),
          div(
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("3. Upload / Sample Input Files"),
              uiOutput(ns("extract_input_status"), inline = TRUE)
            ),
            sec_hint("Upload the Vulsens / Secmod Combination file (.csv) that defines your classification and description columns. Optionally, upload a Region file (.csv) to generate byRegion outputs. The sample files below demonstrate the expected format."),
            fileInput(ns("main_file"), "Vulsens / Secmod Combination File CSV", accept = ".csv", width = "100%"),
            fileInput(ns("region_file"), "Region File CSV (Optional)", accept = ".csv", width = "100%"),
            actionButton(ns("use_samples"), "Use included sample files", class = "primary-btn"),
            div(class = "small-note", textOutput(ns("sample_note"))),
            div(
              class = "sample-row",
              downloadButton(ns("download_sample_main"), "Sample Main CSV"),
              downloadButton(ns("download_sample_region"), "Sample Region CSV")
            )
          ),
          uiOutput(ns("version1_ui")),
          uiOutput(ns("version2_ui")),
          div(
            class = "sidebar-card",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4("4. Extract"),
              uiOutput(ns("extract_extract_status"), inline = TRUE)
            ),
            actionButton(ns("run_extract"), "Create Result Files", class = "create-btn"),
            tags$hr(class = "soft"),
            uiOutput(ns("download_choice_ui")),
            downloadButton(ns("download_output_zip"), "Download", class = "primary-btn")
          )
        ),
        div(
          class = "right-panel",
          uiOutput(ns("status_ui")),
          div(
            class = "info-panel",
            h3("About this section"),
            p("This application enables extraction of Vulnerability Sensitivity (Vulsens) and Secondary Modifiers (Secmod) results from RMS and Verisk model environments. The extracted outputs are subsequently used in generating analytical HTML reports for model evaluation and validation purposes."),
            p("The typical workflow within the application is outlined below:"),
            tags$ol(
              tags$li("Select the modelling platform (RMS or Verisk)."),
              tags$li("Choose the Exposure Database (EDM/TSE) and Result Database (RDM/TSR)."),
              tags$li("Load the required ExposureSet and ResultSet information."),
              tags$li("Configure model version settings and analysis identifiers."),
              tags$li("Generate standardized output datasets at Location, State, County and optional Region levels."),
              tags$li("Review input files, selected metadata and generated outputs using the interactive preview tabs."),
              tags$li("Download individual output files or a consolidated ZIP package for reporting and further analysis.")
            )
          ),
          # ---- Progress stepper placeholder (updated server-side) ----
          uiOutput(ns("progress_panel")),
          
          tabsetPanel(
            tabPanel("ExposureSet", br(), uiOutput(ns("exposure_tabs_ui"))),
            tabPanel("ResultSet", br(), uiOutput(ns("result_tabs_ui"))),
            tabPanel("Input Preview", br(),
                     fluidRow(
                       column(6, preview_table(ns("main_preview"), "Combination File")),
                       column(6, preview_table(ns("region_preview"), "Region File"))
                     )),
            tabPanel("Output Preview", br(), uiOutput(ns("output_tabs_ui")))
          )
        )
      )
    )
  )
}

# Helper UI component
preview_table <- function(id, title) {
  div(class = "preview-card", h4(title), DTOutput(id))
}

# Helper functions for dynamic panels (will be used in server)
db_select_block <- function(ns, i, db_type = c("edm", "rdm")) {
  db_type <- match.arg(db_type)
  input_id <- paste0(db_type, "_db_", i)
  output_id <- paste0(db_type, "_db_selected_", i)
  label <- if (db_type == "edm") "Exposure Database" else "Result Database"
  tagList(
    selectInput(ns(input_id), label,
                choices = character(0),
                selected = NULL,
                width = "100%", selectize = FALSE),
    div(class = "selected-db-display", textOutput(ns(output_id), inline = TRUE))
  )
}

version_panel <- function(ns, i, vendor) {
  div(
    class = "sidebar-card",
    h4(paste0("4.", i, " Version ", i, " Database Settings")),
    sec_hint("Connect to databases, load ExposureSet/ResultSet, and enter IDs."),
    textInput(ns(paste0("version_label_", i)), "Version label",
              value = default_version_label(vendor, i), width = "100%"),
    textInput(ns(paste0("server_", i)), "Server",
              value = default_server(vendor, i), width = "100%"),
    actionButton(ns(paste0("connect_", i)), paste0("Connect version ", i), class = "primary-btn"),
    div(class = "small-note", textOutput(ns(paste0("conn_status_", i)))),
    db_select_block(ns, i, "edm"),
    actionButton(ns(paste0("load_exp_", i)), "Load ExposureSet", class = "primary-btn"),
    textInput(ns(paste0("portinfoid_", i)), "PORTINFOID / ExposureSetSID", value = "", width = "100%"),
    db_select_block(ns, i, "rdm"),
    actionButton(ns(paste0("load_res_", i)), "Load ResultSet", class = "primary-btn"),
    textInput(ns(paste0("anlsid_", i)), "ANLSID / AnalysisResult", value = "", width = "100%")
  )
}