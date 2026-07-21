# server/input_preparation_server.R

inputPrepServer <- function(input, output, session) {
  ns <- session$ns
  
  # Start with sample files enabled (used for previews/downloads), but the
  # "Load Data" stage should only turn green once the user explicitly acts -
  # either by pressing "Use included sample files" or by uploading BOTH
  # files - not simply because sample mode is the default on page load.
  use_samples <- reactiveVal(TRUE)
  data_loaded_confirmed <- reactiveVal(FALSE)
  generated <- reactiveVal(NULL)
  
  # ---- Status badges for left panel ----
  output$prep_settings_status <- renderUI({
    ok <- isTruthy(input$vendor) && isTruthy(input$module) && isTruthy(input$version)
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  output$prep_exposure_status <- renderUI({
    ok <- isTruthy(input$country) &&
      isTruthy(input$peril) &&
      isTruthy(input$subperil) &&
      isTruthy(input$suffix) &&
      !is.null(input$bldgVal) &&
      !is.null(input$cntVal) &&
      !is.null(input$BIVal)
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  output$prep_input_status <- renderUI({
    ok <- !is.null(generated())
    if (ok) {
      tags$span(class = "sec2-status done", icon("check"), "Done")
    } else {
      tags$span(class = "sec2-status pending", icon("clock"), "Pending")
    }
  })
  
  # If user uploads a file, switch to uploaded data
  observeEvent(input$location_file, {
    if (!is.null(input$location_file)) use_samples(FALSE)
  })
  observeEvent(input$combination_file, {
    if (!is.null(input$combination_file)) use_samples(FALSE)
  })
  
  # Once BOTH uploaded files are present, treat data as explicitly loaded
  observe({
    if (!is.null(input$location_file) && !is.null(input$combination_file)) {
      data_loaded_confirmed(TRUE)
    }
  })
  
  # "Use included sample files" button resets to samples
  observeEvent(input$use_samples, {
    use_samples(TRUE)
    data_loaded_confirmed(TRUE)
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
    choices <- peril_lookup()[[input$peril]]
    selectInput(ns("subperil"), "Subperil",
                choices = choices,
                selected = choices[1],
                width = "100%", selectize = FALSE)
  })
  
  current_paths <- reactive({
    sample_paths_input(input$vendor, input$module)
  })
  
  location_data <- reactive({
    sp <- current_paths()
    path <- if (isTRUE(use_samples()) || is.null(input$location_file)) {
      sp$location
    } else {
      input$location_file$datapath
    }
    if (is.null(path) || !is.character(path) || !nzchar(path) || !file.exists(path)) {
      return(data.frame(Message = paste("Location file not found:", path)))
    }
    fread(path)
  })
  
  combination_data <- reactive({
    sp <- current_paths()
    path <- if (isTRUE(use_samples()) || is.null(input$combination_file)) {
      sp$combination
    } else {
      input$combination_file$datapath
    }
    if (is.null(path) || !is.character(path) || !nzchar(path) || !file.exists(path)) {
      return(data.frame(Message = paste("Combination file not found:", path)))
    }
    fread(path)
  })
  
  output$sample_note <- renderText({
    sp <- current_paths()
    paste("Included samples:", basename(sp$location), "and", basename(sp$combination))
  })
  
  output$status_ui <- renderUI({
    ok <- TRUE
    msg <- "Upload location and combination files, or click 'Use included sample files'."
    if (isTRUE(use_samples())) {
      msg <- "Bundled sample files are active for the current vendor/module selection."
    } else if (!is.null(input$location_file) && !is.null(input$combination_file)) {
      msg <- "Uploaded input files are ready for Create Input Files."
    }
    if (!is.null(generated())) {
      msg <- sprintf("Created %s.csv and %s.csv", generated()$locfilename, generated()$accfilename)
    }
    div(class = paste("status-box", if (ok) "success" else "error"), msg)
  })
  
  output$input_dashboard_ui <- renderUI({
    fluidRow(
      column(6, input_preview_table(ns("loc_preview"), "Locations")),
      column(6, input_preview_table(ns("comb_preview"), "Combinations"))
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
      column(6, output_preview_table(ns("location_output_preview"), "Location File",
                                     ns("download_location_output"), "Download Location CSV")),
      column(6, output_preview_table(ns("account_output_preview"), "Account File",
                                     ns("download_account_output"), "Download Account CSV"))
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
  
  # ---- Progress stepper (right panel) ----
  output$progress_panel <- renderUI({
    # Statuses using isTruthy to handle NULL/empty safely
    settings_ok <- isTruthy(input$vendor) &&
      isTruthy(input$module) &&
      isTruthy(input$version)
    
    exposure_ok <- isTruthy(input$country) &&
      isTruthy(input$peril) &&
      isTruthy(input$subperil) &&
      isTruthy(input$suffix) &&
      !is.null(input$bldgVal) &&
      !is.null(input$cntVal) &&
      !is.null(input$BIVal)
    
    # "Load Data" is done once the user has explicitly loaded data in
    # section 3 - either by pressing "Use included sample files" or by
    # uploading BOTH the location and combination files. Sample mode being
    # the default on page load does NOT count until the user acts.
    load_data_ok <- data_loaded_confirmed() &&
      (isTRUE(use_samples()) ||
         (!is.null(input$location_file) && !is.null(input$combination_file)))
    
    # "Input Files" only turns green once "Create Input Files" (the last
    # button in section 3) has actually been pressed and succeeded.
    input_files_ok <- !is.null(generated())
    
    stages <- list(
      list(label = "Settings", done = settings_ok),
      list(label = "Exposure", done = exposure_ok),
      list(label = "Load Data", done = load_data_ok),
      list(label = "Input Files", done = input_files_ok)
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