
################################################################################
# FILE  10 OF 12
################################################################################
# Name: VulsenAPP.R
# Path: VulsenAPP.R
# Type: R
# Size: 0 MB
# Lines: 119
################################################################################


# App for Vulnerability Sensitivity Study


# by Nikil Pujari and Swapan Masanta from Property Research
# =============================================================================
# VulsenAPP.R
# Standalone test launcher for App3 / VulSen module before MetaAPP integration
# =============================================================================


options(shiny.maxRequestSize = 1024 * 1024^2)
options(scipen = 999)


# -----------------------------------------------------------------------------
# Required packages
# -----------------------------------------------------------------------------
required_pkgs <- c(
  "shiny",
  "data.table",
  "dplyr",
  "tidyr",
  "ggplot2",
  "grid",
  "zip",
  "DT",
  "htmltools",
  "plotly",
  "tools",
  "reshape2",
  "scales",
  "knitr"
)


missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]


if (length(missing_pkgs) > 0) {
  stop(
    "Install missing packages first: ",
    paste(missing_pkgs, collapse = ", ")
  )
}


library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(zip)
library(DT)
library(htmltools)
library(plotly)
library(reshape2)
library(scales)


# -----------------------------------------------------------------------------
# Optional: set working directory to this file's folder when run from RStudio
# -----------------------------------------------------------------------------
set_vulsen_wd <- function() {
  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    p <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    
    if (nzchar(p) && file.exists(p)) {
      setwd(dirname(p))
      message("Working directory set to: ", getwd())
      return(invisible(getwd()))
    }
  }
  
  invisible(getwd())
}


set_vulsen_wd()


# -----------------------------------------------------------------------------
# Source VulSen utility files
# These are based on your App3 folder structure.
# -----------------------------------------------------------------------------
source("R/VulsenAPP_utils/VulsenAPP_config.R")
source("R/VulsenAPP_utils/VulsenAPP_ui_helpers.R")
source("R/VulsenAPP_utils/VulsenAPP_data_logic.R")
source("R/VulsenAPP_utils/VulsenAPP_plot_functions.R")
source("R/VulsenAPP_utils/VulsenAPP_html_report.R")


# -----------------------------------------------------------------------------
# Source modular UI/server
# -----------------------------------------------------------------------------
source("ui/VulSen_ui.R")
source("server/Vulsen_server.R")


# -----------------------------------------------------------------------------
# Standalone app
# -----------------------------------------------------------------------------
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "Meta_styles.css"
    )
  ),
  VulSen_ui("vulsen")
)


server <- function(input, output, session) {
  Vulsen_server("vulsen")
}


shiny::shinyApp(ui = ui, server = server)













