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
      shinyjs::useShinyjs(),
      
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
      
      # ---- Cart click handler ----
      shiny::tags$script(shiny::HTML(sprintf("
        function vulCartClick(key) {
          Shiny.setInputValue('%s', {key: key, nonce: Math.random()}, {priority: 'event'});
        }
      ", ns("vul_cart_click")))),
      
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
    
    # ---- Colour swatch click handler ----
    shiny::tags$script(shiny::HTML(paste0("
      $(document).on('click', '.vul-colour-swatch', function(e) {
        var row = $(this).data('row');
        var tag = $(this).closest('.vul-legend-card').data('tag') || 'rel';
        Shiny.setInputValue('", ns(""), "' + tag + '_colour_row', row);
        Shiny.setInputValue('", ns(""), "' + tag + '_colour_tag', tag);
      });
    "))),
    
    # ---- Colour swatch fields sync ----
    shiny::tags$script(shiny::HTML("
      var secmodColourProbeEl = null;
      function secmodResolveToRGB(colourStr) {
        if (!secmodColourProbeEl) {
          secmodColourProbeEl = document.createElement('div');
          secmodColourProbeEl.style.display = 'none';
          document.body.appendChild(secmodColourProbeEl);
        }
        secmodColourProbeEl.style.color = '';
        secmodColourProbeEl.style.color = colourStr;
        if (!secmodColourProbeEl.style.color) return null;
        var computed = getComputedStyle(secmodColourProbeEl).color;
        var m = computed.match(/rgba?\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)/);
        if (!m) return null;
        return { r: parseInt(m[1], 10), g: parseInt(m[2], 10), b: parseInt(m[3], 10) };
      }
      function secmodContrastText(hex) {
        if (!hex) return null;
        var val = hex.trim();
        var h = val.replace('#', '');
        if (h.length === 3) h = h.split('').map(function(c) { return c + c; }).join('');
        var r, g, b;
        if (h.length === 6 && !/[^0-9a-fA-F]/.test(h)) {
          r = parseInt(h.substr(0, 2), 16);
          g = parseInt(h.substr(2, 2), 16);
          b = parseInt(h.substr(4, 2), 16);
        } else {
          var rgb = secmodResolveToRGB(val);
          if (!rgb) return null;
          r = rgb.r; g = rgb.g; b = rgb.b;
        }
        var luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
        return luminance > 0.6 ? '#1a1a1a' : '#ffffff';
      }
      function secmodSyncSwatch(el) {
        var wrap = el.closest('.sec2-swatch-field');
        if (!wrap) return;
        var val = el.value || el.getAttribute('data-init-value') || el.getAttribute('value');
        if (!val) return;
        wrap.style.setProperty('--swatch-color', val);
        var textColor = secmodContrastText(val);
        if (textColor) wrap.style.setProperty('--swatch-text', textColor);
      }
      
      function secmodSyncAllSwatches() {
        document.querySelectorAll('.sec2-swatch-field input.shiny-colour-input')
          .forEach(secmodSyncSwatch);
      }
      
      function reinitColourPickers(container) {
        $(container).find('.shiny-colour-input').each(function() {
          var $input = $(this);
          if ($input.data('minicolors')) {
            $input.minicolors('destroy');
          }
          $input.minicolors({
            theme: 'bootstrap',
            format: 'hex',
            swatches: [],
            position: 'bottom left',
            control: 'hue',
            change: function(hex, opacity) {
              $input.val(hex).trigger('change');
            }
          });
        });
      }
      
      $(document).on('input change', '.sec2-swatch-field input.shiny-colour-input', function() {
        secmodSyncSwatch(this);
      });

      secmodSyncAllSwatches();
      setTimeout(secmodSyncAllSwatches, 300);
      setTimeout(secmodSyncAllSwatches, 1000);
      $(document).on('shiny:connected', secmodSyncAllSwatches);

      var secmodSwatchObserver = new MutationObserver(function() {
        secmodSyncAllSwatches();
      });
      if (document.body) {
        secmodSwatchObserver.observe(document.body, { childList: true, subtree: true });
      }

      Shiny.addCustomMessageHandler('sync-colour-swatches', function(msg) {
        if (typeof secmodSyncAllSwatches === 'function') {
          secmodSyncAllSwatches();
        }
      });

      var secmodSwatchPollCount = 0;
      var secmodSwatchPoll = setInterval(function() {
        secmodSyncAllSwatches();
        secmodSwatchPollCount++;
        if (secmodSwatchPollCount > 40) clearInterval(secmodSwatchPoll);
      }, 500);
    ")),
    
    shiny::div(
      class = "app-shell vulsen-app-shell",
      
      # ---- Header ----
      shiny::div(
        class = "app-header",
        shiny::div(class = "header-top", shiny::h1("Vulnerability Sensitivity")),
        shiny::div(
          class = "header-sub",
          shiny::p("Review Moody's / Verisk vulnerability sensitivity outputs with responsive heatmaps, hover values, a configurable legend, processed-data preview, and downloadable outputs.")
        )
      ),
      
      # ---- Body ----
      shiny::div(
        class = "app-body",
        
        # ----- LEFT PANEL -----
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
            shiny::selectInput(ns("n_models"), "Number of model versions", choices = N_MODEL_CHOICES, selected = "1", width = "100%"),
            shiny::textInput(ns("country_code"), "Country code", value = DEFAULT_COUNTRY_CODE, width = "100%"),
            shiny::textInput(ns("suffix"), "Suffix", value = DEFAULT_SUFFIX, width = "100%"),
            shiny::selectInput(ns("peril"), "Peril", choices = PERIL_CHOICES, selected = "SCS", width = "100%"),
            shiny::selectInput(ns("subperil"), "Subperil", choices = NULL, selected = "AllPeril", width = "100%"),
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
            sec_hint("Upload a CSV file, or click 'Load Included Sample Data' to use the bundled example."),
            shiny::fileInput(ns("file"), "Upload input file", accept = ".csv"),
            shiny::actionButton(ns("load_sample"), "Load Included Sample Data", class = "primary-btn"),
            shiny::div(
              class = "sample-row",
              shiny::downloadButton(ns("download_sample_moody"), "Moody's sample", class = "primary-btn"),
              shiny::downloadButton(ns("download_sample_verisk"), "Verisk sample", class = "primary-btn")
            ),
            
            # ---- Optional region mapping ----
            shiny::tags$hr(class = "soft"),
            sec_hint("Optionally upload a CSV mapping state/province codes to regions. Required if you want Regionwise and Percentage Change plots."),
            shiny::fileInput(ns("region_map_file"), "Upload Regional Mapping (Optional)", accept = ".csv"),
            shiny::actionButton(ns("load_sample_region"), "Load Sample Region Mapping", class = "primary-btn"),
            div(
              class = "sample-row",
              shiny::downloadButton(ns("download_sample_region"), "Sample Region Mapping", class = "primary-btn")
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
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h4("4. Downloads"),
              shiny::uiOutput(ns("download_status"), inline = TRUE)
            ),
            sec_hint("Choose the output type and click the button to download. 'Download Customized HTML' applies your live plot customizations; 'Everything' also bundles both legend configurations as JSON plus the customized HTML report."),
            shiny::selectInput(
              ns("download_type"), NULL,
              choices = c(
                "Download Customized HTML" = "html_customized",
                "All Plots"                = "plots",
                "Processed Data"           = "data",
                "Everything"               = "all"
              ),
              selected = "data"
            ),
            shiny::downloadButton(ns("download_selected"), "Download Selected Output", class = "primary-btn", style = "width:100%; margin-top:8px;")
          ),
          
          # Spacer so the Downloads card (last item) isn't flush against
          # the bottom edge of the scrollable left panel.
          shiny::div(style = "height: 60px;")
        ),
        
        # ----- RIGHT PANEL -----
        shiny::div(
          class = "right-panel",
          
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
              shiny::uiOutput(ns("processed_metadata")),
              DT::DTOutput(ns("processed_table"))
            ),
            # Regionwise and Percentage Change tabs will be hidden/shown dynamically by server
            shiny::tabPanel(
              title = "Regionwise",
              value = "region_tab",
              shiny::br(),
              shiny::uiOutput(ns("region_gallery_controls")),
              shiny::div(class = "vul-gallery-grid", shiny::uiOutput(ns("region_plots_ui")))
            ),
            shiny::tabPanel(
              title = "Statewise",
              shiny::br(),
              shiny::uiOutput(ns("state_gallery_controls")),
              shiny::div(class = "vul-gallery-grid", shiny::uiOutput(ns("state_plots_ui")))
            ),
            shiny::tabPanel(
              title = "Percentage Change",
              value = "pct_tab",
              shiny::br(),
              shiny::uiOutput(ns("pct_gallery_controls")),
              shiny::div(class = "vul-gallery-grid", shiny::uiOutput(ns("pct_plots_ui")))
            )
          )
        )
      )
    )
  )
}