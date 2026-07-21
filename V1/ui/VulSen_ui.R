
# =============================================================================
# ui/VulSen_ui.R
# VulSen UI – MetaApp‑style two‑column layout with override controls
# =============================================================================

VulSen_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "Meta_styles.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),
      shiny::tags$script(src = "toast_notification.js"),
      # ---- Gallery collapse/expand toggle (was missing, so panels never opened) ----
      shiny::tags$script(shiny::HTML("
        function toggleGallery(el) {
          var bar = el.closest('.sec-gallery-bar');
          if (bar) {
            bar.classList.toggle('sec-gallery-collapsed');
            var icon = el.querySelector('i');
            if (icon) {
              icon.classList.toggle('fa-chevron-down');
              icon.classList.toggle('fa-chevron-up');
            }
          }
        }
      ")),
      # ---- Override: make header subtitle use full width ----
      shiny::tags$style(HTML("
        .app-header .header-sub p {
          max-width: 100% !important;
        }
      "))
    ),
    
    shiny::div(
      class = "app-shell",
      
      # ---- Header (no logo) ----
      shiny::div(
        class = "app-header",
        shiny::div(
          class = "header-top",
          shiny::h1("Vulnerability Sensitivity")
        ),
        shiny::div(
          class = "header-sub",
          shiny::p(
            "Review Moody's / Verisk vulnerability sensitivity outputs with responsive heatmaps, hover values, processed‑data preview, and downloadable outputs."
          )
        )
      ),
      
      # ---- Body (two‑column) ----
      shiny::div(
        class = "app-body",
        
        # ----- LEFT PANEL (controls) -----
        shiny::div(
          class = "left-scroll-panel",
          
          # ---- 1. Model Selection ----
          shiny::div(
            id = ns("model-card"),
            class = "sidebar-card",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("1. Model Selection"),
              shiny::uiOutput(ns("model_status"), inline = TRUE)
            ),
            sec_hint("Select the vendor model family and load your input data."),
            shiny::radioButtons(
              inputId = ns("model_family"),
              label   = "Vendor model family",
              choices = c("Moody's / RMS" = "moody", "Verisk" = "verisk"),
              selected = "moody"
            ),
            shiny::uiOutput(ns("version_text")),
            shiny::tags$hr(class = "soft")
          ),
          
          # ---- 2. Input Data ----
          shiny::div(
            id = ns("data-card"),
            class = "sidebar-card",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("2. Input Data"),
              shiny::uiOutput(ns("data_status"), inline = TRUE)
            ),
            sec_hint("Upload a CSV, TXT, or RDS file, or click 'Load Included Sample Data' to use the bundled example."),
            shiny::fileInput(
              inputId = ns("file"),
              label   = "Upload input file",
              accept  = c(".csv", ".txt", ".rds")
            ),
            shiny::actionButton(
              inputId = ns("load_sample"),
              label   = "Load Included Sample Data",
              class   = "primary-btn"
            ),
            shiny::div(
              class = "sample-row",
              shiny::downloadButton(ns("download_sample_moody"), "Moody's sample", class = "primary-btn"),
              shiny::downloadButton(ns("download_sample_verisk"), "Verisk sample", class = "primary-btn")
            ),
            shiny::tags$hr(class = "soft")
          ),
          
          # ---- 3. Plot Options ----
          shiny::div(
            id = ns("options-card"),
            class = "sidebar-card",
            shiny::h4("3. Plot Options"),
            sec_hint("Toggle side‑by‑side focus to emphasise the new version vs. comparison version."),
            shiny::checkboxInput(
              inputId = ns("side_by_side"),
              label   = "Side‑by‑side comparison focus",
              value   = FALSE
            ),
            shiny::checkboxInput(
              inputId = ns("show_hover_help"),
              label   = "Interactive hover help",
              value   = TRUE
            ),
            shiny::tags$hr(class = "soft")
          ),
          
          # ---- 4. Downloads ----
          shiny::div(
            id = ns("download-card"),
            class = "sidebar-card",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("4. Downloads"),
              shiny::uiOutput(ns("download_status"), inline = TRUE)
            ),
            sec_hint("Choose the output type and click the button to download."),
            shiny::selectInput(
              inputId = ns("download_type"),
              label   = NULL,
              choices = c(
                "HTML Report"    = "html",
                "All Plots"      = "plots",
                "Processed Data" = "data",
                "Everything"     = "all"
              ),
              selected = "data"
            ),
            shiny::downloadButton(
              outputId = ns("download_selected"),
              label    = "Download Selected Output",
              class    = "primary-btn",
              style    = "width:100%; margin-top:8px;"
            )
          )
        ),
        
        # ----- RIGHT PANEL (output) -----
        shiny::div(
          class = "right-panel",
          
          # ---- Status ----
          shiny::uiOutput(ns("status_ui")),
          
          # ---- Info panel ----
          shiny::div(
            class = "info-panel",
            shiny::h3("About this module"),
            shiny::tags$ul(
              shiny::tags$li("Supports Moody's / RMS and Verisk vulnerability sensitivity comparison files."),
              shiny::tags$li("Builds regionwise, statewise, and percentage‑change heatmap galleries."),
              shiny::tags$li("Each plot can be customised with size, colours, labels, and legend settings."),
              shiny::tags$li("Hover over cells to see exact relative AAL or percentage change values.")
            )
          ),
          
          # ---- Progress stepper ----
          shiny::uiOutput(ns("progress_panel")),
          
          # ---- Tabs ----
          shiny::tabsetPanel(
            id = ns("vulsen_tabs"),
            shiny::tabPanel(
              title = "Regionwise",
              shiny::br(),
              shiny::uiOutput(ns("region_gallery_controls")),
              shiny::uiOutput(ns("region_plots_ui"))
            ),
            shiny::tabPanel(
              title = "Statewise",
              shiny::br(),
              shiny::uiOutput(ns("state_gallery_controls")),
              shiny::uiOutput(ns("state_plots_ui"))
            ),
            shiny::tabPanel(
              title = "Percentage Change",
              shiny::br(),
              shiny::uiOutput(ns("pct_gallery_controls")),
              shiny::uiOutput(ns("pct_plots_ui"))
            ),
            shiny::tabPanel(
              title = "Processed Data",
              shiny::br(),
              DT::DTOutput(ns("processed_table"))
            ),
            shiny::tabPanel(
              title = "Notes",
              shiny::br(),
              shiny::div(
                class = "info-panel",
                shiny::h3("Notes"),
                shiny::p("If validation fails, check that the selected model family matches the uploaded comparison file and that the required columns are present.")
              )
            )
          )
        )
      )
    )
  )
}






