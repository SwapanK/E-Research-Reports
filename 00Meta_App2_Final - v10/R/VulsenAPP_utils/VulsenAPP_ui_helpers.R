# =============================================================================
# VulsenAPP_ui_helpers.R - UI helpers for VulSen app
# =============================================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (is.atomic(a) && length(a) == 1) {
    if (is.na(a) || (is.character(a) && !nzchar(a))) return(b)
  }
  a
}

# -----------------------------------------------------------------------------
# sec_safe_id_key() - sanitize a category/key value (e.g. "Construction Class")
# before it is pasted into an HTML id/input-id string.
#
# Category keys come straight from the data (peril names, model class labels,
# etc.) and can contain spaces, slashes, parentheses, etc. Any such character
# pasted raw into an id is technically legal HTML, but it silently breaks the
# moment ANYTHING treats that id as a CSS/jQuery selector string (a single
# space turns "#foo_Construction Class" into a descendant-combinator
# selector "#foo_Construction" + tag "Class", which matches nothing). This
# is what caused the per-plot Data Label Colour override panel to fail to
# initialise its colourpicker widget for any category whose name contains a
# space, while the Gallery Defaults swatch (built from a plain "prefix",
# no category key) worked fine.
#
# IMPORTANT: Vulsen_server.R must build/read the *identical* sanitized id
# for these inputs/outputs (e.g. input[[paste0(prefix, "_data_label_colour_",
# sec_safe_id_key(key))]]) - if the server keeps using the raw `key` while
# the UI now uses the sanitized one, the ids will no longer match at all.
# -----------------------------------------------------------------------------
sec_safe_id_key <- function(key) {
  safe <- gsub("[^A-Za-z0-9_-]+", "_", key)   # anything not alnum/_/- -> "_"
  safe <- gsub("_+", "_", safe)               # collapse repeated underscores
  safe <- gsub("^_|_$", "", safe)             # trim leading/trailing "_"
  safe
}

# -----------------------------------------------------------------------------
# Header / branding
# -----------------------------------------------------------------------------
render_header <- function(title, subtitle = NULL) {
  htmltools::tags$div(
    class = "header-panel",
    htmltools::tags$div(
      class = "brand",
      if (file.exists("www/images/Gallagher-logo-large.png")) {
        htmltools::tags$img(src = "images/Gallagher-logo-large.png", alt = "Gallagher logo")
      },
      if (file.exists("www/images/GallagherRe_StackedLarge-3D.png")) {
        htmltools::tags$img(src = "images/GallagherRe_StackedLarge-3D.png", alt = "Gallagher Re logo")
      }
    ),
    htmltools::tags$div(
      class = "meta",
      htmltools::tags$h1(title),
      htmltools::tags$p(
        "Interactive Moody's / Verisk vulnerability sensitivity review with Rmd logic, ",
        "responsive heatmaps, hover tooltips, side-by-side comparison toggle, and HTML export."
      ),
      if (!is.null(subtitle)) htmltools::tags$p(subtitle)
    )
  )
}

# -----------------------------------------------------------------------------
# Metric card
# -----------------------------------------------------------------------------
metric_card <- function(value, label) {
  htmltools::tags$div(
    class = "metric-card",
    htmltools::tags$div(class = "metric-value", value),
    htmltools::tags$div(class = "metric-label", label)
  )
}

# -----------------------------------------------------------------------------
# Status box
# -----------------------------------------------------------------------------
status_box <- function(type = c("warning", "error", "success"), text) {
  type <- match.arg(type)
  cls <- switch(type, warning = "warning-box", error = "error-box", success = "success-box")
  htmltools::tags$div(class = cls, text)
}

# -----------------------------------------------------------------------------
# Plot card (simple, no gallery controls - used elsewhere if needed)
# -----------------------------------------------------------------------------
plot_card <- function(output_id, title, caption = NULL, height = "900px", ns = identity) {
  htmltools::tags$div(
    class = "plot-card",
    htmltools::tags$h3(title),
    shiny::plotOutput(ns(output_id), height = height),
    if (!is.null(caption) && nzchar(caption)) htmltools::tags$div(class = "plot-caption", caption)
  )
}

# -----------------------------------------------------------------------------
# Dynamic plot height based on number of rows/columns
# -----------------------------------------------------------------------------
plot_height_for <- function(scope = c("region", "state", "pct"), n_y = 8, n_x = 8) {
  scope <- match.arg(scope)
  if (scope == "state") return(paste0(max(960, min(1800, 300 + 21 * n_y + 18 * n_x)), "px"))
  if (scope == "pct") return(paste0(max(760, min(1180, 280 + 20 * n_y + 16 * n_x)), "px"))
  paste0(max(760, min(1180, 320 + 18 * n_y + 14 * n_x)), "px")
}

# -----------------------------------------------------------------------------
# Lightbulb hint
# -----------------------------------------------------------------------------
sec_hint <- function(text) {
  div(class = "sec2-hint", span(class = "sec2-hint-icon", icon("lightbulb")), span(text))
}

# -----------------------------------------------------------------------------
# sec_control_section - helper for grouped controls
# -----------------------------------------------------------------------------
sec_control_section <- function(title, ..., section_class = "sec2-control-section") {
  div(
    class = section_class,
    div(class = "sec2-control-section-title", title),
    div(class = "sec2-control-section-grid", ...)
  )
}

# -----------------------------------------------------------------------------
# Legend Configuration Manager UI
# -----------------------------------------------------------------------------
vul_legend_config_ui <- function(ns, prefix, title, editable = TRUE, readonly_note = NULL) {
  if (!isTRUE(editable)) {
    return(
      sec_control_section(
        title,
        section_class = "sec2-control-section vul-legend-section",
        div(
          style = "grid-column: 1 / -1;",
          if (!is.null(readonly_note)) div(class = "vul-legend-readonly-note", readonly_note),
          shiny::uiOutput(ns(paste0(prefix, "_legend_preview")))
        )
      )
    )
  }
  
  sec_control_section(
    title,
    section_class = "sec2-control-section vul-legend-section",
    div(
      class = "vul-legend-card",
      `data-tag` = prefix,
      
      div(
        class = "vul-legend-toolbar-group vul-legend-toolbar-main",
        div(
          class = "vul-legend-toolbar-row vul-legend-toolbar-row-merged",
          actionButton(
            ns(paste0(prefix, "_legend_load_default")), "Load Default",
            icon = icon("rotate-left"), class = "sec2-btn"
          ),
          div(
            class = "vul-compact-file",
            fileInput(
              ns(paste0(prefix, "_legend_json_file")), NULL,
              accept = ".json", buttonLabel = "Load JSON",
              placeholder = "No file", width = "220px"
            )
          ),
          div(class = "vul-legend-toolbar-divider"),
          numericInput(
            ns(paste0(prefix, "_legend_n_bins")), "Number of Bins",
            value = 9, min = 2, max = 15, step = 1, width = "130px"
          ),
          actionButton(
            ns(paste0(prefix, "_legend_create_bins")), "Create Bins from Data",
            icon = icon("wand-magic-sparkles"), class = "sec2-btn"
          )
        )
      ),
      
      shiny::uiOutput(ns(paste0(prefix, "_legend_msg"))),
      
      div(
        class = "vul-legend-toolbar-group",
        div(class = "vul-legend-toolbar-label", "Edit Bins"),
        div(class = "vul-legend-table-wrap", DT::DTOutput(ns(paste0(prefix, "_legend_table"))))
      ),
      
      div(
        class = "vul-legend-toolbar-group",
        div(class = "vul-legend-toolbar-label", "Legend Preview"),
        shiny::uiOutput(ns(paste0(prefix, "_legend_preview")))
      ),
      
      div(
        class = "vul-legend-toolbar-group vul-legend-toolbar-export",
        div(
          class = "vul-legend-toolbar-row",
          downloadButton(
            ns(paste0(prefix, "_legend_download_json")), "Download JSON",
            class = "sec2-btn"
          )
        )
      )
    )
  )
}

# ---- Legend preview swatches ----
vul_legend_preview_tags <- function(scheme_df) {
  df <- scheme_df[order(-suppressWarnings(as.numeric(scheme_df$level))), ]
  htmltools::tags$div(
    class = "vul-legend-preview-row",
    lapply(seq_len(nrow(df)), function(i) {
      htmltools::tags$div(
        class = "vul-legend-chip",
        htmltools::tags$span(class = "vul-legend-swatch", style = paste0("background:", df$colour[i], ";")),
        htmltools::tags$span(class = "vul-legend-chip-label", df$label[i])
      )
    })
  )
}

# ---- Black or white text for readability ----
vul_contrast_text <- function(hex) {
  hex <- sub("^#", "", ifelse(is.na(hex) | !nzchar(hex), "FFFFFF", hex))
  hex <- ifelse(nchar(hex) == 6, hex, "FFFFFF")
  r <- strtoi(substr(hex, 1, 2), base = 16L)
  g <- strtoi(substr(hex, 3, 4), base = 16L)
  b <- strtoi(substr(hex, 5, 6), base = 16L)
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  ifelse(is.na(lum), "#1c2534", ifelse(lum > 0.55, "#1c2534", "#ffffff"))
}

# ---- Editable legend DT with colour swatches ----
vul_legend_datatable <- function(scheme_df) {
  df <- scheme_df[order(-suppressWarnings(as.numeric(scheme_df$level))), c("level", "label", "lower", "upper", "colour")]
  
  df$lower <- ifelse(is.infinite(df$lower), as.character(df$lower), format(df$lower, trim = TRUE))
  df$upper <- ifelse(is.infinite(df$upper), as.character(df$upper), format(df$upper, trim = TRUE))
  
  names(df) <- c("Level", "Label", "Lower", "Upper", "Colour")
  
  header_html <- htmltools::tags$table(
    class = "display",
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th("Level"),
        htmltools::tags$th("Label"),
        htmltools::tags$th(htmltools::HTML("<i class='fa fa-pencil'></i> Lower")),
        htmltools::tags$th(htmltools::HTML("<i class='fa fa-lock'></i> Upper")),
        htmltools::tags$th("Colour")
      )
    )
  )
  
  DT::datatable(
    df,
    container = header_html,
    editable = list(target = "cell", disable = list(columns = c(0, 3))),
    rownames = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        # Lower column: text cursor
        list(
          targets = 2,
          createdCell = DT::JS(
            "function(td, cellData, rowData, row, col) {",
            "  $(td).css('cursor', 'text');",
            "}"
          )
        ),
        # Upper column: greyed out
        list(
          targets = 3,
          createdCell = DT::JS(
            "function(td, cellData, rowData, row, col) {",
            "  $(td).css({",
            "    'background-color': '#f5f5f5',",
            "    'color': '#888',",
            "    'cursor': 'default'",
            "  });",
            "}"
          )
        ),
        # Colour column: clickable swatch with hex text
        list(
          targets = 4,
          render = DT::JS(
            "function(data, type, row, meta) {",
            "  if (type === 'display') {",
            "    var hex = data || '#898D8D';",
            "    var r = parseInt(hex.substring(1,3), 16);",
            "    var g = parseInt(hex.substring(3,5), 16);",
            "    var b = parseInt(hex.substring(5,7), 16);",
            "    var lum = (0.299 * r + 0.587 * g + 0.114 * b) / 255;",
            "    var textColor = (lum > 0.55) ? '#1c2534' : '#ffffff';",
            "    return '<div class=\"vul-colour-swatch\" ' +",
            "                'style=\"background:' + hex + '; width:100%; height:30px; border-radius:4px; cursor:pointer; border:1px solid #ccc; display:flex; align-items:center; justify-content:center; font-size:11px; font-weight:600; color:' + textColor + ';\" ' +",
            "                'data-row=\"' + (meta.row + 1) + '\" ' +",
            "                'data-colour=\"' + hex + '\"' +",
            "                'title=\"Click to change colour\">' + hex + '</div>';",
            "  }",
            "  return data;",
            "}"
          )
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Gallery controls bar (collapsible, "Apply to all" button)
# -----------------------------------------------------------------------------
vul_gallery_controls_ui <- function(
    ns,
    prefix,
    n,
    default_width_in = 9,
    default_height_in = 5,
    default_dpi = 300,
    default_transparent_bg = FALSE,
    default_panel_gap_px = 16,
    default_top_margin_px = 10,
    default_bottom_margin_px = 10,
    default_left_margin_px = 10,
    default_right_margin_px = 10,
    default_axis_text = 12,
    default_x_rotation = 0,
    default_x_vjust = 0.5,
    default_plot_title = 16,
    default_strip_text = 12,
    default_legend_text = 8,
    default_legend_title = 7,
    default_legend_key_size = 0.5,
    default_show_labels = TRUE,
    default_data_label_size = 3.5,
    default_data_label_colour = "#FFFFFF",
    has_facets = TRUE,
    title = "Gallery defaults",
    legend_ui = NULL
) {
  
  div(
    class = "sec-gallery-bar sec-gallery-collapsed",
    div(
      class = "sec-gallery-bar-head",
      div(class = "sec-gallery-bar-title", icon("sliders-h"), title),
      div(
        style = "display:flex; align-items:center; gap:8px;",
        span(class = "sec-gallery-bar-count", paste(n, "plots")),
        tags$button(class = "sec-gallery-toggle", onclick = "toggleGallery(this)", icon("chevron-down"))
      )
    ),
    div(
      class = "sec-gallery-body",
      sec_hint("Changes take effect after clicking \u2018Apply to all\u2019 below."),
      div(
        class = "sec-gallery-controls",
        
        # Problem 3: Plot Width/Height/DPI/Transparent Background/Gap are the
        # single WYSIWYG canvas config driving screen render AND every
        # export path - the old Export Width/Height (in) + Heatmap Display
        # Width (%) + Heatmap Display Height (px) + Plot Card Max Width (px)
        # controls (three independent sizing systems) are gone.
        sec_control_section(
          "Output / Canvas Properties",
          numericInput(ns(paste0(prefix, "_default_width_in")), "Plot Width (in)", value = default_width_in, min = 3, max = 20, step = 0.5, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_height_in")), "Plot Height (in)", value = default_height_in, min = 2, max = 15, step = 0.5, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_dpi")), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
          checkboxInput(ns(paste0(prefix, "_default_transparent_bg")), "Transparent background", value = default_transparent_bg),
          # Model Panel Gap controls the horizontal space between the two
          # model facets (e.g. HDv1 vs RLv25) inside a single heatmap - only
          # meaningful when the plot actually has facets to push apart.
          if (has_facets) numericInput(ns(paste0(prefix, "_default_panel_gap_px")), "Model Panel Gap (px)", value = default_panel_gap_px, min = 0, max = 80, step = 2, width = "100px")
        ),
        
        # Problem 3: margins are exposed in Gallery Defaults only (no
        # per-plot override inputs) and apply consistently across every
        # gallery plot.
        sec_control_section(
          "Plot Margins",
          numericInput(ns(paste0(prefix, "_default_top_margin_px")), "Top Margin (px)", value = default_top_margin_px, min = 0, max = 100, step = 2, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_bottom_margin_px")), "Bottom Margin (px)", value = default_bottom_margin_px, min = 0, max = 100, step = 2, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_left_margin_px")), "Left Margin (px)", value = default_left_margin_px, min = 0, max = 100, step = 2, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_right_margin_px")), "Right Margin (px)", value = default_right_margin_px, min = 0, max = 100, step = 2, width = "100px")
        ),
        
        # Problem 5: no Axis Title control - axis titles are never shown.
        # Problem 6: no Strip Font Face control - strip text is always bold.
        # Problem 2: X-Axis Label Rotation / VJust replace those removed
        # controls with something that actually has visual impact.
        sec_control_section(
          "Axis / Title / Strip Text",
          numericInput(ns(paste0(prefix, "_default_axis_text")), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_x_rotation")), "X-Axis Label Rotation (deg)", value = default_x_rotation, min = -360, max = 360, step = 5, width = "110px"),
          numericInput(ns(paste0(prefix, "_default_x_vjust")), "X-Axis Label VJust", value = default_x_vjust, min = 0, max = 1, step = 0.1, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_plot_title")), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
          if (has_facets) numericInput(ns(paste0(prefix, "_default_strip_text")), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
        ),
        
        # Problem 7: no Legend Position control - legend is always top when shown.
        sec_control_section(
          "Legend Properties",
          checkboxInput(ns(paste0(prefix, "_legend_show")), "Show legend", value = TRUE),
          numericInput(ns(paste0(prefix, "_default_legend_text")), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_title")), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_key_size")), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
        ),
        
        # Problem 8: no Data Label Font Face control - data labels are always bold.
        sec_control_section(
          "Data Label Properties",
          checkboxInput(ns(paste0(prefix, "_default_show_labels")), "Show value labels", value = default_show_labels),
          numericInput(ns(paste0(prefix, "_default_data_label_size")), "Data label size", value = default_data_label_size, min = 1, max = 10, step = 0.5, width = "100px"),
          div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_default_data_label_colour")), "Data label colour", value = default_data_label_colour, showColour = "both", width = "100px"))
        ),
        
        if (!is.null(legend_ui)) legend_ui
      ),
      
      div(
        class = "sec-gallery-bar-foot",
        actionButton(ns(paste0(prefix, "_apply_all")), paste0("Apply to all ", n, " plots"), icon = icon("wand-magic-sparkles"), class = "sec2-btn")
      )
    ),
    # ---- Force swatch sync after this UI is rendered ----
    tags$script(HTML("setTimeout(secmodSyncAllSwatches, 100);"))
  )
}

# -----------------------------------------------------------------------------
# Plot card with override panel (for each individual plot)
# -----------------------------------------------------------------------------
vul_plot_card_gallery <- function(
    ns,
    key,
    label,
    prefix,
    default_width_in = 9,
    default_height_in = 5,
    default_dpi = 300,
    default_transparent_bg = FALSE,
    default_panel_gap_px = 16,
    default_axis_text = 12,
    default_x_rotation = 0,
    default_x_vjust = 0.5,
    default_plot_title = 16,
    default_strip_text = 12,
    default_legend_text = 8,
    default_legend_title = 7,
    default_legend_key_size = 0.5,
    default_show_labels = TRUE,
    default_data_label_size = 3.5,
    default_data_label_colour = "#FFFFFF",
    has_facets = TRUE
) {
  
  # Sanitize the category key before it ever becomes part of an id string.
  # `key` itself (e.g. "Construction Class") is still used for the visible
  # `label` and is preserved verbatim in `data-category` below, but every
  # id/input-id from here down uses `safe_key`.
  safe_key <- sec_safe_id_key(key)
  ov_id <- ns(paste0(prefix, "_override_", safe_key))
  
  div(
    class = "cart-item-card vul-plot-card",
    style = "margin-bottom:14px;",
    
    div(
      class = "cart-item-header",
      span(class = "cart-item-badge", label),
      div(
        style = "display:flex; gap:4px;",
        tags$button(
          onclick = sprintf(
            "var _ovp = document.getElementById('%s'); 
             _ovp.classList.toggle('sec-open'); 
             if (_ovp.classList.contains('sec-open')) { 
               if (typeof reinitColourPickers === 'function') {
                 setTimeout(function() { 
                   reinitColourPickers(_ovp); 
                   if (typeof secmodSyncAllSwatches === 'function') secmodSyncAllSwatches(); 
                 }, 30);
               }
             }",
            ov_id
          ),
          class = "btn-icon-cart sec2-icon-btn", title = "Adjust size, text & legend",
          icon("sliders-h")
        ),
        downloadButton(
          outputId = ns(paste0(prefix, "_dl_", safe_key)), label = NULL, icon = icon("download"),
          class = "btn-icon-cart sec2-icon-btn", title = "Download"
        ),
        tags$button(
          onclick = sprintf("vulCartClick('%s|%s')", prefix, safe_key),
          class = "btn-icon-cart sec2-icon-btn", title = "Add to cart",
          icon("cart-plus")
        )
      )
    ),
    
    div(class = "sec-plot-frame", uiOutput(ns(paste0(prefix, "_plot_frame_", safe_key)))),
    
    div(
      id = ov_id,
      class = "sec-override-panel",
      `data-category` = key,
      
      # Problem 3: Plot Width/Height (in) + DPI + Transparent Background +
      # Model Panel Gap - same shape as the Gallery Defaults canvas
      # section, minus margins (margins stay Gallery-Defaults-only).
      sec_control_section(
        "Output / Canvas Properties",
        numericInput(ns(paste0(prefix, "_width_in_", safe_key)), "Plot Width (in)", value = default_width_in, min = 3, max = 20, step = 0.5, width = "100px"),
        numericInput(ns(paste0(prefix, "_height_in_", safe_key)), "Plot Height (in)", value = default_height_in, min = 2, max = 15, step = 0.5, width = "100px"),
        numericInput(ns(paste0(prefix, "_dpi_", safe_key)), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
        checkboxInput(ns(paste0(prefix, "_transparent_bg_", safe_key)), "Transparent background", value = default_transparent_bg),
        # Model Panel Gap pushes the model facets (e.g. HDv1 vs RLv25) apart
        # horizontally - only meaningful when the plot actually has facets.
        if (has_facets) numericInput(ns(paste0(prefix, "_panel_gap_px_", safe_key)), "Model Panel Gap (px)", value = default_panel_gap_px, min = 0, max = 80, step = 2, width = "100px")
      ),
      
      # Problem 5/6: no Axis Title / Strip Font Face controls.
      # Problem 2: X-Axis Label Rotation / VJust.
      sec_control_section(
        "Axis / Title / Strip Text",
        numericInput(ns(paste0(prefix, "_axis_text_", safe_key)), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_x_rotation_", safe_key)), "X-Axis Label Rotation (deg)", value = default_x_rotation, min = -360, max = 360, step = 5, width = "110px"),
        numericInput(ns(paste0(prefix, "_x_vjust_", safe_key)), "X-Axis Label VJust", value = default_x_vjust, min = 0, max = 1, step = 0.1, width = "100px"),
        numericInput(ns(paste0(prefix, "_plot_title_", safe_key)), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
        if (has_facets) numericInput(ns(paste0(prefix, "_strip_text_", safe_key)), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
      ),
      
      # Problem 7: no Legend Position control - always top when shown.
      sec_control_section(
        "Legend Properties",
        checkboxInput(ns(paste0(prefix, "_legend_show_", safe_key)), "Show legend", value = TRUE),
        numericInput(ns(paste0(prefix, "_legend_text_", safe_key)), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_title_", safe_key)), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_key_size_", safe_key)), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
      ),
      
      # Problem 8: no Data Label Font Face control - always bold.
      sec_control_section(
        "Data Label Properties",
        checkboxInput(ns(paste0(prefix, "_show_labels_", safe_key)), "Show value labels", value = default_show_labels),
        numericInput(ns(paste0(prefix, "_data_label_size_", safe_key)), "Data label size", value = default_data_label_size, min = 1, max = 10, step = 0.5, width = "100px"),
        div(class = "sec2-swatch-field", colourInput(ns(paste0(prefix, "_data_label_colour_", safe_key)), "Data label colour", value = default_data_label_colour, showColour = "both", width = "100px"))
      )
    ),
    # ---- Force swatch sync after this UI is rendered ----
    tags$script(HTML("setTimeout(secmodSyncAllSwatches, 100);"))
  )
}


