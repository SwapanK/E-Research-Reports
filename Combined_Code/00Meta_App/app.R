# app.R
options(shiny.maxRequestSize = 1024 * 1024^2)

# Load required packages
required_pkgs <- c("shiny", "data.table", "dplyr", "DT", "RODBC", "glue", "stringr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install missing packages: ", paste(missing_pkgs, collapse = ", "))

library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(RODBC)
library(glue)
library(stringr)

# Source helper functions
source("R/helpers.R")

# ---------- Module: Input File Creation ----------
inputFileCreationUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    div(class = "app-shell",
        div(class = "app-header",
            div(class = "header-top",
                h1("Input File Creation App for Vulnerability Sensitivity and Secondary Modifier Modules"),
                img(src = "GallagherRe_StackedLarge-3D.png", class = "header-logo")
            ),
            div(class = "header-sub",
                p("Create, review and download standardized model input files for RMS and Verisk platforms")
            )
        ),
        div(class = "app-body",
            div(class = "left-scroll-panel",
                div(class = "sidebar-card",
                    h4("1. Model Selection"),
                    selectInput(ns("vendor"), "Vendor model family",
                                choices = c("Moody's", "Verisk"), selected = "Moody's",
                                width = "100%", selectize = FALSE),
                    uiOutput(ns("version_ui")),
                    selectInput(ns("module"), "Module",
                                choices = c("Vulnerability Sensitivity", "Secondary Modifiers"),
                                selected = "Vulnerability Sensitivity",
                                width = "100%", selectize = FALSE),
                    tags$hr(class = "soft"),
                    h4("2. Exposure Settings"),
                    textInput(ns("country"), "Country code", value = "US", width = "100%"),
                    selectInput(ns("peril"), "Peril",
                                choices = names(peril_lookup()), selected = "SCS",
                                width = "100%", selectize = FALSE),
                    uiOutput(ns("subperil_ui")),
                    textInput(ns("suffix"), "Suffix", value = "2026", width = "100%"),
                    numericInput(ns("bldgVal"), "Building value", value = 1000000, min = 0, width = "100%"),
                    numericInput(ns("cntVal"), "Contents value", value = 500000, min = 0, width = "100%"),
                    numericInput(ns("BIVal"), "BI value", value = 100000, min = 0, width = "100%"),
                    tags$hr(class = "soft"),
                    h4("3. Upload / Sample Input Files"),
                    fileInput(ns("location_file"), "Locations File CSV", accept = c(".csv"), width = "100%"),
                    fileInput(ns("combination_file"), "Combinations File CSV", accept = c(".csv"), width = "100%"),
                    actionButton(ns("use_samples"), "Use included sample files", class = "primary-btn"),
                    div(class = "small-note", textOutput(ns("sample_note"))),
                    div(class = "sample-row",
                        downloadButton(ns("download_sample_location"), "Sample Locations"),
                        downloadButton(ns("download_sample_combination"), "Sample Combinations")
                    ),
                    tags$hr(class = "soft"),
                    actionButton(ns("create_files"), "Create Input Files", class = "create-btn")
                )
            ),
            div(class = "right-panel",
                uiOutput(ns("status_ui")),
                div(class = "info-panel",
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
                tabsetPanel(
                  tabPanel("Input Dashboard", br(), uiOutput(ns("input_dashboard_ui"))),
                  tabPanel("Output Dashboard", br(), uiOutput(ns("output_dashboard_ui")))
                )
            )
        )
    )
  )
}

inputFileCreationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    use_samples <- reactiveVal(FALSE)
    generated <- reactiveVal(NULL)
    
    observeEvent(input$use_samples, {
      use_samples(TRUE)
      showNotification("Using bundled sample files for the current vendor/module selection.", type = "message")
    })
    
    output$version_ui <- renderUI({
      if (input$vendor == "Moody's") {
        textInput(ns("version"), "Model version", value = "HD", width = "100%")
      } else {
        textInput(ns("version"), "Model version", value = "v13", width = "100%")
      }
    })
    
    output$subperil_ui <- renderUI({
      selectInput(ns("subperil"), "Subperil",
                  choices = peril_lookup()[[input$peril]],
                  selected = peril_lookup()[[input$peril]][1],
                  width = "100%", selectize = FALSE)
    })
    
    current_paths <- reactive({
      sample_paths_input(input$vendor, input$module)
    })
    
    location_data <- reactive({
      sp <- current_paths()
      if (isTRUE(use_samples()) || is.null(input$location_file)) {
        fread(sp$location)
      } else {
        fread(input$location_file$datapath)
      }
    })
    
    combination_data <- reactive({
      sp <- current_paths()
      if (isTRUE(use_samples()) || is.null(input$combination_file)) {
        fread(sp$combination)
      } else {
        fread(input$combination_file$datapath)
      }
    })
    
    output$status_ui <- renderUI({
      msg <- "Upload location and combination files, or click 'Use included sample files'."
      if (isTRUE(use_samples())) {
        msg <- "Bundled sample files are active for the current vendor/module selection."
      } else if (!is.null(input$location_file) && !is.null(input$combination_file)) {
        msg <- "Uploaded input files are ready for Create Input Files."
      }
      if (!is.null(generated())) {
        msg <- sprintf("Created %s.csv and %s.csv", generated()$locfilename, generated()$accfilename)
      }
      div(class = paste("status-box", "success"), msg)
    })
    
    output$input_dashboard_ui <- renderUI({
      fluidRow(
        column(6, div(class = "preview-card", h4("Locations"), DTOutput(ns("loc_preview")))),
        column(6, div(class = "preview-card", h4("Combinations"), DTOutput(ns("comb_preview"))))
      )
    })
    
    output$loc_preview <- renderDT({
      datatable(head(location_data(), 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    output$comb_preview <- renderDT({
      datatable(head(combination_data(), 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    observeEvent(input$create_files, {
      tryCatch({
        res <- build_files(
          vendor = input$vendor,
          module = input$module,
          version = input$version,
          country = input$country,
          peril = input$peril,
          subperil = input$subperil,
          suffix = input$suffix,
          bldgVal = input$bldgVal,
          cntVal = input$cntVal,
          BIVal = input$BIVal,
          latlon = location_data(),
          combination = combination_data()
        )
        generated(res)
        showNotification("Input files generated successfully.", type = "message")
      }, error = function(e) {
        generated(NULL)
        showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })
    
    output$output_dashboard_ui <- renderUI({
      req(generated())
      fluidRow(
        column(6,
               div(class = "preview-card",
                   div(class = "preview-toolbar", h4("Location File"), downloadButton(ns("download_location_output"), "Download Location CSV")),
                   DTOutput(ns("location_output_preview"))
               )),
        column(6,
               div(class = "preview-card",
                   div(class = "preview-toolbar", h4("Account File"), downloadButton(ns("download_account_output"), "Download Account CSV")),
                   DTOutput(ns("account_output_preview"))
               ))
      )
    })
    
    output$location_output_preview <- renderDT({
      req(generated())
      datatable(head(generated()$loc_comb, 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    output$account_output_preview <- renderDT({
      req(generated())
      datatable(head(generated()$acc_comb, 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    output$download_sample_location <- downloadHandler(
      filename = function() basename(current_paths()$location),
      content = function(file) file.copy(current_paths()$location, file, overwrite = TRUE)
    )
    output$download_sample_combination <- downloadHandler(
      filename = function() basename(current_paths()$combination),
      content = function(file) file.copy(current_paths()$combination, file, overwrite = TRUE)
    )
    output$download_location_output <- downloadHandler(
      filename = function() paste0(generated()$locfilename, ".csv"),
      content = function(file) fwrite(generated()$loc_comb, file)
    )
    output$download_account_output <- downloadHandler(
      filename = function() paste0(generated()$accfilename, ".csv"),
      content = function(file) fwrite(generated()$acc_comb, file)
    )
  })
}

# ---------- Module: Result Extraction ----------
resultExtractionUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    div(class = "app-shell",
        div(class = "app-header",
            div(class = "header-top",
                h1("Result Extraction App for Vulnerability Sensitivity and Secondary Modifier Modules"),
                img(src = "GallagherRe_StackedLarge-3D.png", class = "header-logo")
            ),
            div(class = "header-sub",
                p("Extract, review and download standardized model outputs from RMS and Verisk environments")
            )
        ),
        div(class = "app-body",
            div(class = "left-scroll-panel",
                div(class = "sidebar-card",
                    h4("1. Model Selection"),
                    selectInput(ns("vendor"), "Vendor model family",
                                choices = c("Moody's", "Verisk"),
                                width = "100%", selectize = FALSE),
                    selectInput(ns("module"), "Module",
                                choices = c("Vulnerability Sensitivity", "Secondary Modifiers"),
                                width = "100%", selectize = FALSE),
                    uiOutput(ns("version_count_ui")),
                    tags$hr(class = "soft"),
                    h4("2. Exposure Settings"),
                    textInput(ns("country"), "Country code", value = "US", width = "100%"),
                    selectInput(ns("peril"), "Peril",
                                choices = names(peril_lookup()), selected = "SCS",
                                width = "100%", selectize = FALSE),
                    uiOutput(ns("subperil_ui")),
                    textInput(ns("suffix"), "Suffix", value = "2026", width = "100%")
                ),
                div(class = "sidebar-card",
                    h4("3. Upload / Sample Input Files"),
                    fileInput(ns("main_file"), "Vulsens / Secmod Combination File CSV", accept = c(".csv"), width = "100%"),
                    fileInput(ns("region_file"), "Region File CSV (Optional)", accept = c(".csv"), width = "100%"),
                    actionButton(ns("use_samples"), "Use included sample files", class = "primary-btn"),
                    div(class = "small-note", textOutput(ns("sample_note"))),
                    div(class = "sample-row",
                        downloadButton(ns("download_sample_main"), "Sample Main CSV"),
                        downloadButton(ns("download_sample_region"), "Sample Region CSV")
                    )
                ),
                uiOutput(ns("version1_ui")),
                uiOutput(ns("version2_ui")),
                div(class = "sidebar-card",
                    actionButton(ns("run_extract"), "Create Result Files", class = "create-btn"),
                    tags$hr(class = "soft"),
                    uiOutput(ns("download_choice_ui")),
                    downloadButton(ns("download_output_zip"), "Download", class = "primary-btn")
                )
            ),
            div(class = "right-panel",
                uiOutput(ns("status_ui")),
                div(class = "info-panel",
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
                tabsetPanel(
                  tabPanel("ExposureSet", br(), uiOutput(ns("exposure_tabs_ui"))),
                  tabPanel("ResultSet", br(), uiOutput(ns("result_tabs_ui"))),
                  tabPanel("Input Preview", br(),
                           fluidRow(
                             column(6, div(class = "preview-card", h4("Combination File"), DTOutput(ns("main_preview")))),
                             column(6, div(class = "preview-card", h4("Region File"), DTOutput(ns("region_preview"))))
                           )),
                  tabPanel("Output Preview", br(), uiOutput(ns("output_tabs_ui")))
                )
            )
        )
    )
  )
}

resultExtractionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    use_samples <- reactiveVal(TRUE)
    conns <- reactiveValues(v1 = NULL, v2 = NULL)
    dbs <- reactiveValues(v1 = character(0), v2 = character(0))
    exp_sets <- reactiveValues(v1 = NULL, v2 = NULL)
    res_sets <- reactiveValues(v1 = NULL, v2 = NULL)
    generated <- reactiveVal(NULL)
    app_status <- reactiveVal("Default sample files are loaded. Connect to server, load ExposureSet/ResultSet, then create result files.")
    app_status_type <- reactiveVal("success")
    
    observeEvent(input$vendor, {
      conns$v1 <- NULL; conns$v2 <- NULL
      exp_sets$v1 <- NULL; exp_sets$v2 <- NULL
      res_sets$v1 <- NULL; res_sets$v2 <- NULL
      generated(NULL)
    })
    
    output$version_count_ui <- renderUI({
      if (module_tag(input$module) == "Vulsens") {
        selectInput(ns("version_count"), "Number of model versions",
                    choices = c("1", "2"), width = "100%", selectize = FALSE)
      }
    })
    
    output$subperil_ui <- renderUI({
      selectInput(ns("subperil"), "Subperil",
                  choices = peril_lookup()[[input$peril]],
                  selected = peril_lookup()[[input$peril]][1],
                  width = "100%", selectize = FALSE)
    })
    
    output$version1_ui <- renderUI({
      version_panel(1, input$vendor, ns)
    })
    output$version2_ui <- renderUI({
      req(module_tag(input$module) == "Vulsens")
      req(input$version_count == "2")
      version_panel(2, input$vendor, ns)
    })
    
    # Helper to generate version panel UI inside module
    version_panel <- function(i, vendor, ns) {
      div(class = "sidebar-card",
          h4(paste0("4.", i, " Version ", i, " Database Settings")),
          textInput(ns(paste0("version_label_", i)), "Version label",
                    value = default_version_label(vendor, i), width = "100%"),
          textInput(ns(paste0("server_", i)), "Server",
                    value = default_server(vendor, i), width = "100%"),
          actionButton(ns(paste0("connect_", i)), paste0("Connect version ", i), class = "primary-btn"),
          div(class = "small-note", textOutput(ns(paste0("conn_status_", i)))),
          db_select_block(i, "edm", ns),
          actionButton(ns(paste0("load_exp_", i)), "Load ExposureSet", class = "primary-btn"),
          textInput(ns(paste0("portinfoid_", i)), "PORTINFOID / ExposureSetSID", value = "", width = "100%"),
          db_select_block(i, "rdm", ns),
          actionButton(ns(paste0("load_res_", i)), "Load ResultSet", class = "primary-btn"),
          textInput(ns(paste0("anlsid_", i)), "ANLSID / AnalysisResult", value = "", width = "100%")
      )
    }
    
    db_select_block <- function(i, db_type, ns) {
      input_id <- paste0(db_type, "_db_", i)
      output_id <- paste0(db_type, "_db_selected_", i)
      label <- if (db_type == "edm") "Exposure Database" else "Result Database"
      tagList(
        selectInput(ns(input_id), label, choices = character(0), selected = NULL,
                    width = "100%", selectize = FALSE),
        div(class = "selected-db-display", textOutput(ns(output_id), inline = TRUE))
      )
    }
    
    current_paths <- reactive({
      sample_paths_extract(input$vendor, input$module)
    })
    
    output$sample_note <- renderText({
      sp <- current_paths()
      paste("Included samples:", basename(sp$main), "and", basename(sp$region))
    })
    
    observeEvent(input$use_samples, {
      use_samples(TRUE)
      app_status("Reset to bundled sample files for the selected vendor/module.")
      app_status_type("success")
    })
    
    main_data <- reactive({
      sp <- current_paths()
      path <- if (isTRUE(use_samples()) || is.null(input$main_file)) sp$main else input$main_file$datapath
      dt <- read_input_csv(path, "Main input file")
      validate_input_file_extract(dt, input$vendor, input$module)
      dt
    })
    
    observeEvent(input$main_file, {
      if (!is.null(input$main_file)) use_samples(FALSE)
    })
    
    region_data <- reactive({
      sp <- current_paths()
      path <- if (isTRUE(use_samples()) && is.null(input$region_file)) sp$region else if (!is.null(input$region_file)) input$region_file$datapath else NULL
      load_region_file(path)
    })
    
    output$status_ui <- renderUI({
      div(class = paste("status-box", app_status_type()), app_status())
    })
    
    output$main_preview <- renderDT({
      datatable(head(main_data(), 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    output$region_preview <- renderDT({
      r <- region_data()
      if (is.null(r)) return(datatable(data.frame(Message = "No region file supplied. byRegion output will be skipped."), rownames = FALSE))
      datatable(head(r, 50), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    output$download_sample_main <- downloadHandler(
      filename = function() basename(current_paths()$main),
      content = function(file) file.copy(current_paths()$main, file, overwrite = TRUE)
    )
    output$download_sample_region <- downloadHandler(
      filename = function() basename(current_paths()$region),
      content = function(file) file.copy(current_paths()$region, file, overwrite = TRUE)
    )
    
    connect_version <- function(i, user = NULL, password = NULL) {
      id <- paste0("v", i)
      server <- input[[paste0("server_", i)]]
      conn <- connect_sql(input$vendor, server, user, password)
      conns[[id]] <- conn
      db_names <- list_databases(conn)
      dbs[[id]] <- db_names
      updateSelectInput(session, paste0("edm_db_", i), choices = db_names, selected = if (length(db_names)) db_names[1] else character(0))
      updateSelectInput(session, paste0("rdm_db_", i), choices = db_names, selected = if (length(db_names)) db_names[1] else character(0))
      app_status(paste0("Connected version ", i, " and loaded database list."))
      app_status_type("success")
    }
    
    for (i in 1:2) {
      local({
        ii <- i
        observeEvent(input[[paste0("connect_", ii)]], {
          if (vendor_tag(input$vendor) == "RMS") {
            showModal(modalDialog(
              title = paste0("RMS credentials for version ", ii),
              textInput(ns(paste0("rms_user_", ii)), "Username", value = Sys.getenv("USERNAME"), width = "100%"),
              passwordInput(ns(paste0("rms_pwd_", ii)), "Password", width = "100%"),
              footer = tagList(modalButton("Cancel"), actionButton(ns(paste0("rms_login_", ii)), "Connect")),
              easyClose = TRUE
            ))
          } else {
            tryCatch(connect_version(ii), error = function(e) { app_status(conditionMessage(e)); app_status_type("error") })
          }
        })
        
        observeEvent(input[[paste0("rms_login_", ii)]], {
          removeModal()
          tryCatch(connect_version(ii, input[[paste0("rms_user_", ii)]], input[[paste0("rms_pwd_", ii)]]),
                   error = function(e) { app_status(conditionMessage(e)); app_status_type("error") })
        })
        
        output[[paste0("conn_status_", ii)]] <- renderText({
          if (is.null(conns[[paste0("v", ii)]])) "Not connected." else "Connected. Select Exposure and Result databases."
        })
        
        output[[paste0("edm_db_selected_", ii)]] <- renderText({
          val <- input[[paste0("edm_db_", ii)]]
          if (is.null(val) || length(val) == 0 || !nzchar(val)) "Selected Exposure Database will appear here after connection." else val
        })
        output[[paste0("rdm_db_selected_", ii)]] <- renderText({
          val <- input[[paste0("rdm_db_", ii)]]
          if (is.null(val) || length(val) == 0 || !nzchar(val)) "Selected Result Database will appear here after connection." else val
        })
        
        observeEvent(input[[paste0("load_exp_", ii)]], {
          tryCatch({
            req(conns[[paste0("v", ii)]], input[[paste0("edm_db_", ii)]])
            exp_sets[[paste0("v", ii)]] <- get_exposure_set(conns[[paste0("v", ii)]], input$vendor, input[[paste0("edm_db_", ii)]])
            app_status(paste0("ExposureSet loaded for version ", ii, ".")); app_status_type("success")
          }, error = function(e) { app_status(conditionMessage(e)); app_status_type("error") })
        })
        
        observeEvent(input[[paste0("load_res_", ii)]], {
          tryCatch({
            req(conns[[paste0("v", ii)]], input[[paste0("rdm_db_", ii)]])
            res_sets[[paste0("v", ii)]] <- get_result_set(conns[[paste0("v", ii)]], input$vendor, input[[paste0("rdm_db_", ii)]])
            app_status(paste0("ResultSet loaded for version ", ii, ".")); app_status_type("success")
          }, error = function(e) { app_status(conditionMessage(e)); app_status_type("error") })
        })
      })
    }
    
    output$exposure_tabs_ui <- renderUI({
      tabs <- list(tabPanel("Version 1", DTOutput(ns("exp_v1"))))
      if (module_tag(input$module) == "Vulsens" && identical(input$version_count, "2")) {
        tabs <- c(tabs, list(tabPanel("Version 2", DTOutput(ns("exp_v2")))))
      }
      do.call(tabsetPanel, tabs)
    })
    
    output$result_tabs_ui <- renderUI({
      tabs <- list(tabPanel("Version 1", DTOutput(ns("res_v1"))))
      if (module_tag(input$module) == "Vulsens" && identical(input$version_count, "2")) {
        tabs <- c(tabs, list(tabPanel("Version 2", DTOutput(ns("res_v2")))))
      }
      do.call(tabsetPanel, tabs)
    })
    
    output$exp_v1 <- renderDT(datatable(exp_sets$v1 %||% data.frame(Message = "Click Load ExposureSet after selecting Exposure Database."),
                                        options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
    output$exp_v2 <- renderDT(datatable(exp_sets$v2 %||% data.frame(Message = "Click Load ExposureSet after selecting Exposure Database."),
                                        options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
    output$res_v1 <- renderDT(datatable(res_sets$v1 %||% data.frame(Message = "Click Load ResultSet after selecting Result Database."),
                                        options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
    output$res_v2 <- renderDT(datatable(res_sets$v2 %||% data.frame(Message = "Click Load ResultSet after selecting Result Database."),
                                        options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
    
    get_versions <- reactive({
      n <- if (module_tag(input$module) == "Vulsens") as.integer(input$version_count %||% "1") else 1L
      lapply(seq_len(n), function(i) {
        id <- paste0("v", i)
        if (is.null(conns[[id]])) stop("Version ", i, " is not connected.")
        list(
          label = trimws(input[[paste0("version_label_", i)]]),
          conn = conns[[id]],
          edm_db = input[[paste0("edm_db_", i)]],
          rdm_db = input[[paste0("rdm_db_", i)]],
          portinfoid = input[[paste0("portinfoid_", i)]],
          anlsid = input[[paste0("anlsid_", i)]]
        )
      })
    })
    
    observeEvent(input$run_extract, {
      tryCatch({
        res <- run_extraction(input$vendor, input$module, main_data(), region_data(),
                              get_versions(), input$country, input$peril, input$subperil, input$suffix)
        generated(res)
        files_created <- paste(names(res$outputs), collapse = ", ")
        app_status(paste0("Result files created: ", files_created, "."))
        app_status_type("success")
      }, error = function(e) {
        generated(NULL)
        msg <- conditionMessage(e)
        if (grepl("portinfoid|ExposureSetSID|Anlsid|ANLSID", msg, ignore.case = TRUE)) {
          msg <- paste(msg, "Please verify the ExposureSet/ResultSet tabs and enter an existing Portinfoid/ExposureSetSID and Anlsid.")
        }
        app_status(msg); app_status_type("error")
        showNotification(msg, type = "error", duration = NULL)
      })
    })
    
    output$download_choice_ui <- renderUI({
      req(generated())
      selectInput(ns("download_choice"), "Output files to download",
                  choices = c(names(generated()$outputs), "All Files"),
                  selected = "All Files", width = "100%", selectize = FALSE)
    })
    
    output$output_tabs_ui <- renderUI({
      req(generated())
      tabs <- lapply(names(generated()$outputs), function(nm) {
        tabPanel(nm,
                 downloadButton(ns(paste0("csv_", nm)), paste0("Download ", nm, " CSV"), class = "output-download-btn"),
                 DTOutput(ns(paste0("out_", nm))))
      })
      do.call(tabsetPanel, tabs)
    })
    
    observe({
      req(generated())
      for (nm in names(generated()$outputs)) {
        local({
          name <- nm
          output[[paste0("out_", name)]] <- renderDT({
            datatable(head(generated()$outputs[[name]], 100),
                      options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
          })
          output[[paste0("csv_", name)]] <- downloadHandler(
            filename = function() generated()$filenames[[name]] %||% paste0(name, ".csv"),
            content = function(file) data.table::fwrite(generated()$outputs[[name]], file)
          )
        })
      }
    })
    
    write_zip <- function(file, choice = "All Files") {
      req(generated())
      td <- tempfile("vulsens_secmod_")
      dir.create(td)
      outs <- generated()$outputs
      if (!identical(choice, "All Files")) outs <- outs[choice]
      written <- character(0)
      for (nm in names(outs)) {
        fname <- generated()$filenames[[nm]] %||% paste0(nm, ".csv")
        fpath <- file.path(td, fname)
        data.table::fwrite(outs[[nm]], fpath)
        written <- c(written, fpath)
      }
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = basename(written))
    }
    
    output$download_output_zip <- downloadHandler(
      filename = function() {
        choice <- input$download_choice %||% "All Files"
        if (identical(choice, "All Files")) {
          paste0("Vulsens_Secmod_AllFiles_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
        } else {
          paste0(choice, ".zip")
        }
      },
      content = function(file) write_zip(file, input$download_choice %||% "All Files")
    )
    
    session$onSessionEnded(function() {
      for (id in c("v1", "v2")) if (!is.null(conns[[id]])) try(RODBC::odbcClose(conns[[id]]), silent = TRUE)
    })
  })
}

# ---------- Main App ----------
ui <- navbarPage(
  title = "Vulsens & Secmod Suite",
  id = "nav",
  tabPanel("Input File Creation", inputFileCreationUI("inputApp")),
  tabPanel("Result Extraction", resultExtractionUI("resultApp")),
  collapsible = TRUE,
  theme = NULL,  # we use custom CSS
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)

server <- function(input, output, session) {
  inputFileCreationServer("inputApp")
  resultExtractionServer("resultApp")
}

shinyApp(ui, server)