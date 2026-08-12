# server/result_extraction_server.R

resultExtractServer <- function(input, output, session) {
  ns <- session$ns
  
  use_samples <- reactiveVal(TRUE)
  input_loaded <- reactiveVal(FALSE)
  
  conns <- reactiveValues(v1 = NULL, v2 = NULL)
  dbs <- reactiveValues(v1 = character(0), v2 = character(0))
  exp_sets <- reactiveValues(v1 = NULL, v2 = NULL)
  res_sets <- reactiveValues(v1 = NULL, v2 = NULL)
  generated <- reactiveVal(NULL)
  app_status <- reactiveVal("Default sample files are loaded. Connect to server, load ExposureSet/ResultSet, then create result files.")
  app_status_type <- reactiveVal("success")
  
  # ---- Status badges for left panel ----
  output$extract_settings_status <- renderUI({
    ok <- isTruthy(input$vendor) && isTruthy(input$module)
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  output$extract_exposure_status <- renderUI({
    ok <- isTruthy(input$country) &&
      isTruthy(input$peril) &&
      isTruthy(input$subperil) &&
      isTruthy(input$suffix)
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  output$extract_input_status <- renderUI({
    ok <- !is.null(main_data()) && input_loaded()
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  output$extract_extract_status <- renderUI({
    ok <- !is.null(generated())
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  # ---- Reset input_loaded when vendor or module changes ----
  observeEvent(input$vendor, {
    conns$v1 <- NULL; conns$v2 <- NULL
    exp_sets$v1 <- NULL; exp_sets$v2 <- NULL
    res_sets$v1 <- NULL; res_sets$v2 <- NULL
    generated(NULL)
    input_loaded(FALSE)
    app_status("Vendor changed. Please reload input files and re-run extraction.")
    app_status_type("info")
  })
  
  observeEvent(input$module, {
    conns$v1 <- NULL; conns$v2 <- NULL
    exp_sets$v1 <- NULL; exp_sets$v2 <- NULL
    res_sets$v1 <- NULL; res_sets$v2 <- NULL
    generated(NULL)
    input_loaded(FALSE)
    app_status("Module changed. Please reload input files and re-run extraction.")
    app_status_type("info")
  })
 
  
  
  output$version_count_ui <- renderUI({
    if (module_tag(input$module) == "Vulsens") {
      selectInput(ns("version_count"), "Number of model versions",
                  choices = c("1", "2"),
                  width = "100%", selectize = FALSE)
    }
  })
  
  output$subperil_ui <- renderUI({
    choices <- peril_lookup()[[input$peril]]
    selectInput(ns("subperil"), "Subperil",
                choices = choices,
                selected = choices[1],
                width = "100%", selectize = FALSE)
  })
  
  output$version1_ui <- renderUI({
    version_panel(ns, 1, input$vendor)
  })
  
  output$version2_ui <- renderUI({
    req(module_tag(input$module) == "Vulsens")
    req(input$version_count == "2")
    version_panel(ns, 2, input$vendor)
  })
  
  current_paths <- reactive({
    sample_paths_extract(input$vendor, input$module)
  })
  
  output$sample_note <- renderText({
    sp <- current_paths()
    paste("Included samples:", basename(sp$main), "and", basename(sp$region))
  })
  
  # ---- Handle "Use included sample files" ----
  observeEvent(input$use_samples, {
    use_samples(TRUE)
    input_loaded(TRUE)     # <-- user deliberately chose samples
    generated(NULL)        # clear any previous extraction results
    app_status("Using bundled sample files. Re-run extraction if needed.")
    app_status_type("info")
    updateTabsetPanel(session, "extract_tabs", selected = "Input Preview")
  })
  
  # ---- Main data (combination file) ----
  main_data <- reactive({
    sp <- current_paths()
    path <- if (isTRUE(use_samples()) || is.null(input$main_file)) {
      sp$main
    } else {
      input$main_file$datapath
    }
    if (is.null(path) || !is.character(path) || !nzchar(path) || !file.exists(path)) {
      return(data.frame(Message = paste("Main input file not found:", path)))
    }
    dt <- read_input_csv(path, "Main input file")
    validate_input_file(dt, input$vendor, input$module)
    dt
  })
  
  # ---- Handle file upload ----
  observeEvent(input$main_file, {
    if (!is.null(input$main_file)) {
      use_samples(FALSE)
      input_loaded(TRUE)   # <-- user uploaded a file
      updateTabsetPanel(session, "extract_tabs", selected = "Input Preview")
    }
  })
  
  # ---- Region data ----
  region_data <- reactive({
    sp <- current_paths()
    if (isTRUE(use_samples()) && is.null(input$region_file)) {
      path <- sp$region
    } else if (!is.null(input$region_file)) {
      path <- input$region_file$datapath
    } else {
      path <- NULL
    }
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      return(NULL)
    }
    load_region_file(path)
  })
  
  output$status_ui <- renderUI({
    div(class = paste("status-box", app_status_type()), app_status())
  })
  
  output$main_preview <- renderDT({
    # NOTE: filter = "top" removed. DT's column-filter row loads its own
    # bundled selectize.js, which clobbers Shiny's global selectize plugin
    # registry (dropping "selectize-plugin-a11y") for the rest of the
    # browser session. That later breaks any selectInput rendered via
    # renderUI elsewhere on the page (e.g. Vulsen's subperil/model-column
    # dropdowns), which silently fail to bind if this table was viewed
    # first. searching = TRUE keeps a global search box without the
    # conflicting per-column selectize filter row.
    datatable(main_data(), options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  
  output$region_preview <- renderDT({
    r <- region_data()
    if (is.null(r)) {
      return(datatable(data.frame(Message = "No region file supplied. byRegion output will be skipped."), rownames = FALSE))
    }
    # filter = "top" removed -- see note in main_preview above.
    datatable(r, options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  
  output$download_sample_main <- downloadHandler(
    filename = function() basename(current_paths()$main),
    content = function(file) file.copy(current_paths()$main, file, overwrite = TRUE)
  )
  
  output$download_sample_region <- downloadHandler(
    filename = function() basename(current_paths()$region),
    content = function(file) file.copy(current_paths()$region, file, overwrite = TRUE)
  )
  
  # ---- Connection helpers ----
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
          updateTabsetPanel(session, "extract_tabs", selected = "ExposureSet")
        }, error = function(e) { app_status(conditionMessage(e)); app_status_type("error") })
      })
      
      observeEvent(input[[paste0("load_res_", ii)]], {
        tryCatch({
          req(conns[[paste0("v", ii)]], input[[paste0("rdm_db_", ii)]])
          res_sets[[paste0("v", ii)]] <- get_result_set(conns[[paste0("v", ii)]], input$vendor, input[[paste0("rdm_db_", ii)]])
          app_status(paste0("ResultSet loaded for version ", ii, ".")); app_status_type("success")
          updateTabsetPanel(session, "extract_tabs", selected = "ResultSet")
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
  
  # filter = "top" removed from exp_v1/exp_v2/res_v1/res_v2 below -- see
  # note in main_preview above re: selectize.js plugin registry conflict.
  output$exp_v1 <- renderDT({
    datatable(exp_sets$v1 %||% data.frame(Message = "Click Load ExposureSet after selecting Exposure Database."),
              options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  output$exp_v2 <- renderDT({
    datatable(exp_sets$v2 %||% data.frame(Message = "Click Load ExposureSet after selecting Exposure Database."),
              options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  output$res_v1 <- renderDT({
    datatable(res_sets$v1 %||% data.frame(Message = "Click Load ResultSet after selecting Result Database."),
              options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  output$res_v2 <- renderDT({
    datatable(res_sets$v2 %||% data.frame(Message = "Click Load ResultSet after selecting Result Database."),
              options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
  })
  
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
      res <- run_extraction(
        vendor = input$vendor,
        module = input$module,
        template = main_data(),
        region_dt = region_data(),
        versions = get_versions(),
        country = input$country,
        peril = input$peril,
        subperil = input$subperil,
        suffix = input$suffix
      )
      generated(res)
      files_created <- paste(names(res$outputs), collapse = ", ")
      app_status(paste0("Result files created: ", files_created, "."))
      app_status_type("success")
      updateTabsetPanel(session, "extract_tabs", selected = "Output Preview")
    }, error = function(e) {
      generated(NULL)
      msg <- conditionMessage(e)
      if (grepl("portinfoid|ExposureSetSID|Anlsid|ANLSID", msg, ignore.case = TRUE)) {
        msg <- paste(msg, "Please verify the ExposureSet/ResultSet tabs and enter an existing Portinfoid/ExposureSetSID and Anlsid.")
      }
      app_status(msg); app_status_type("error")
      showNotification(msg, type = "error", duration = 10)
    })
  })
  
  output$download_choice_ui <- renderUI({
    req(generated())
    selectInput(ns("download_choice"), "Output files to download",
                choices = c(names(generated()$outputs), "All Files"),
                selected = "All Files",
                width = "100%", selectize = FALSE)
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
          # filter = "top" removed -- see note in main_preview above re:
          # selectize.js plugin registry conflict.
          datatable(generated()$outputs[[name]],
                    options = list(pageLength = 10, scrollX = TRUE, searching = TRUE), rownames = FALSE)
        })
        output[[paste0("csv_", name)]] <- downloadHandler(
          filename = function() generated()$filenames[[name]] %||% paste0(name, ".csv"),
          content = function(file) fwrite(generated()$outputs[[name]], file)
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
      fwrite(outs[[nm]], fpath)
      written <- c(written, fpath)
    }
    old <- setwd(td); on.exit(setwd(old), add = TRUE)
    zip(zipfile = file, files = basename(written))
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
    for (id in c("v1", "v2")) {
      if (!is.null(isolate(conns[[id]]))) try(odbcClose(isolate(conns[[id]])), silent = TRUE)
    }
  })
  
  # ---- Progress stepper (right panel) ----
  output$progress_panel <- renderUI({
    # Stage 1: Settings
    settings_ok <- isTruthy(input$vendor) && isTruthy(input$module)
    
    # Stage 2: Exposure
    exposure_ok <- isTruthy(input$country) &&
      isTruthy(input$peril) &&
      isTruthy(input$subperil) &&
      isTruthy(input$suffix)
    
    # Stage 3: Input Files – green ONLY after user explicitly loaded files
    input_files_ok <- !is.null(main_data()) && input_loaded()
    
    # Stage 4: Extract – green ONLY after successful extraction
    extract_ok <- !is.null(generated())
    
    stages <- list(
      list(label = "Settings", done = settings_ok),
      list(label = "Exposure", done = exposure_ok),
      list(label = "Input Files", done = input_files_ok),
      list(label = "Extract", done = extract_ok)
    )
    
    div(
      class = "sec-progress-panel",
      style = "margin: 16px 0 12px 0; padding: 12px 16px; background: #f8faff; border-radius: 16px; border: 1px solid #e2e8f0;",
      div(
        style = "display: flex; align-items: center; justify-content: space-between; gap: 4px;",
        lapply(seq_along(stages), function(i) {
          st <- stages[[i]]
          is_done <- st$done
          circle <- if (is_done) {
            tags$span(class = "sec-step-circle done", icon("check"))
          } else {
            tags$span(class = "sec-step-circle pending", i)
          }
          label <- tags$div(class = "sec-step-label", st$label)
          connector <- if (i < length(stages)) {
            tags$div(
              class = paste("sec-connector", if (is_done && stages[[i+1]]$done) "done" else "pending")
            )
          } else NULL
          tags$div(
            class = "sec-step-clickable",
            style = "display: flex; align-items: center; flex: 1;",
            tags$div(
              style = "display: flex; flex-direction: column; align-items: center;",
              circle,
              label
            ),
            connector
          )
        })
      )
    )
  })
}