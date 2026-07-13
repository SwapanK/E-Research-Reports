# MetaApp.R - Combined Shiny Application

options(shiny.maxRequestSize = 1024 * 1024^2)

# Load required packages
library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(RODBC)
library(glue)
library(stringr)

# Source UI and server modules
source("ui/input_preparation_ui.R")
source("ui/result_extraction_ui.R")
source("server/input_preparation_server.R")
source("server/result_extraction_server.R")

# Main UI
ui <- navbarPage(
  title = "Vulsens & Secmod Toolkit",
  id = "main_nav",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "Meta_styles.css")
  ),
  tabPanel("Input File Creation", inputPrepUI("prep")),
  tabPanel("Result Extraction", resultExtractUI("extract"))
)

# Main Server
server <- function(input, output, session) {
  callModule(inputPrepServer, "prep")
  callModule(resultExtractServer, "extract")
}

shinyApp(ui, server)