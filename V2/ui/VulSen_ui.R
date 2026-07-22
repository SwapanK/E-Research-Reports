# =============================================================================
# ui/VulSen_ui.R
# VulSen UI - MetaApp-style two-column layout with override controls +
# Legend Configuration Manager
# =============================================================================

VulSen_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "Meta_styles.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "secmod_style.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "dashboard_style.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "vulsen_style.css"),
      shiny::tags$script(src = "toast_notification.js"),
      
      # ---- Gallery collapse/expand toggle ----
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
            if (!bar.classList.contains('sec-gallery-collapsed')) {
              setTimeout(function() {
                if (window.jQuery) {
                  $(bar).find('table.dataTable').each(function() {
                    var dt = $(this).DataTable();
                    if (dt) { dt.columns.adjust().draw(false); }
                  });
                }
                window.dispatchEvent(new Event('resize'));
              }, 0);
            }
          }
        }
      ")),
      
      # ---- Local style overrides ----
      shiny::tags$style(HTML("
        .app-header .header-sub p {
          max-width: 100% !important;
        }
        .vul-legend-readonly-note {
          font-size: 12px;
          color: #6b7280;
          font-style: italic;
          margin-bottom: 8px;
        }
        .vul-legend-section {
          grid-column: 1 / -1;
        }
        .vul-colour-swatch {
          user-select: none;
        }
      "))
    ),
    
    # ---- Colour swatch click handler (with namespace) ----
    shiny::tags$script(shiny::HTML(paste0("
      $(document).on('click', '.vul-colour-swatch', function(e) {
        var row = $(this).data('row');
        var tag = $(this).closest('.vul-legend-card').data('tag') || 'rel';
        Shiny.setInputValue('", ns(""), "' + tag + '_colour_row', row);
        Shiny.setInputValue('", ns(""), "' + tag + '_colour_tag', tag);
      });
    "))),
    
    shiny::div(
      class = "app-shell vulsen-app-shell",
      
      # ---- Header (no logo) ----
      shiny::div(
        class = "app-header",
        shiny::div(class = "header-top", shiny::h1("Vulnerability Sensitivity")),
        shiny::div(
          class = "header-sub",
          shiny::p("Review Moody's / Verisk vulnerability sensitivity outputs with responsive heatmaps, hover values, a configurable legend, processed-data preview, and downloadable outputs.")
        )
      ),
      
      # ---- Body (two-column) ----
      shiny::div(
        class = "app-body",
        
        # ----- LEFT PANEL (controls) -----
        shiny::div(
          class = "left-scroll-panel",
          
          # ---- 1. Model Configuration ----
          shiny::div(
            id = ns("model-card"),
            class = "sidebar-card",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("1. Model Configuration"),
              shiny::uiOutput(ns("model_status"), inline = TRUE)
            ),
            sec_hint("Select the vendor, how many model versions to compare, and the peril/geography settings for this run."),
            shiny::selectInput(ns("model_family"), "Vendor model family", choices = VENDOR_CHOICES, selected = "moody", width = "100%"),
            shiny::selectInput(ns("n_models"), "Number of model versions", choices = N_MODEL_CHOICES, selected = "2", width = "100%"),
            shiny::textInput(ns("country_code"), "Country code", value = DEFAULT_COUNTRY_CODE, width = "100%"),
            shiny::textInput(ns("suffix"), "Suffix", value = DEFAULT_SUFFIX, width = "100%"),
            shiny::selectInput(ns("peril"), "Peril", choices = PERIL_CHOICES, selected = "SCS", width = "100%"),
            shiny::selectInput(ns("subperil"), "Subperil", choices = SUBPERIL_CHOICES, selected = "AllPeril", width = "100%"),
            shiny::tags$hr(class = "soft"),
            shiny::uiOutput(ns("model_columns_ui")),
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
            shiny::fileInput(ns("file"), "Upload input file", accept = c(".csv", ".txt", ".rds")),
            shiny::actionButton(ns("load_sample"), "Load Included Sample Data", class = "primary-btn"),
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
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("3. Plot Options"),
              shiny::uiOutput(ns("plot_status"), inline = TRUE)
            ),
            sec_hint("Once your model configuration and data are ready, click below to build the plots. Legend bin edits (see each tab's Gallery Defaults) update the plots live afterwards, without needing to click this again."),
            shiny::actionButton(ns("create_plot"), "Create Plot", class = "primary-btn", style = "width:100%;"),
            shiny::tags$hr(class = "soft")
          ),
          
          # ---- 4. Downloads ----
          shiny::div(
            id = ns("download-card"),
            class = "sidebar-card",
            shiny::h4("4. Downloads"),
            sec_hint("Choose the output type and click the button to download. 'Everything' also bundles both legend configurations as JSON."),
            shiny::selectInput(
              ns("download_type"), NULL,
              choices = c("HTML Report" = "html", "All Plots" = "plots", "Processed Data" = "data", "Everything" = "all"),
              selected = "data"
            ),
            shiny::downloadButton(ns("download_selected"), "Download Selected Output", class = "primary-btn", style = "width:100%; margin-top:8px;")
          )
        ),
        
        # ----- RIGHT PANEL (output) -----
        shiny::div(
          class = "right-panel",
          
          shiny::uiOutput(ns("status_ui")),
          
          shiny::div(
            class = "info-panel",
            shiny::h3("About this module"),
            shiny::tags$ul(
              shiny::tags$li("Supports Moody's / RMS and Verisk vulnerability sensitivity comparison files."),
              shiny::tags$li("Builds regionwise, statewise, and percentage-change heatmap galleries, each using one consistent full legend."),
              shiny::tags$li("Legend Configuration Manager: edit bins/colours by hand, auto-generate quantile bins from the current data, or import/export a legend as JSON - Relative AAL (Regionwise/Statewise) and Percentage Change are configured independently."),
              shiny::tags$li("Each plot can be customised with size, text, legend, and data-label settings."),
              shiny::tags$li("Hover over cells to see exact relative AAL or percentage change values.")
            )
          ),
          
          shiny::uiOutput(ns("progress_panel")),
          
          shiny::tabsetPanel(
            id = ns("vulsen_tabs"),
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
                shiny::p("If validation fails, check that the selected model family matches the uploaded comparison file and that the required columns are present."),
                shiny::p("The Relative AAL legend is shared by the Regionwise and Statewise tabs - edit it once on Regionwise and it applies everywhere. The Percentage Change legend is fully independent.")
              )
            ),
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
              value = "pct_tab",
              shiny::br(),
              shiny::uiOutput(ns("pct_gallery_controls")),
              shiny::uiOutput(ns("pct_plots_ui"))
            )
          )
        )
      )
    )
  )
}



