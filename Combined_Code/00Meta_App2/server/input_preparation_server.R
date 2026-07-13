# Server module for Input File Creation App

inputPrepServer <- function(input, output, session) {
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
    choices <- peril_lookup()[[input$peril]]
    selectInput(ns("subperil"), "Subperil",
                choices = choices,
                selected = choices[1],
                width = "100%", selectize = FALSE)
  })
  
  current_paths <- reactive({
    sample_paths(input$vendor, input$module)
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
    ok <- TRUE
    msg <- "Upload location and combination files, or click 'Use included sample files'."
    if (isTRUE(use_samples())) {
      msg <- "Bundled sample files are active for the current vendor/module selection."
    } else if (!is.null(input$location_file) && !is.null(input$combination_file)) {
      msg <- "Uploaded input files are ready for CreateInputFiles."
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
}