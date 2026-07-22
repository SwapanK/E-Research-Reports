# =============================================================================
# VulsenAPP_ui_helpers.R - UI helpers for VulSen app
# =============================================================================

`%||%` <- function(a, b) {
  # NULL / zero-length (e.g. an unset input) -> fall back.
  if (is.null(a) || length(a) == 0) return(b)
  # Only apply the extra "blank scalar" checks (NA, empty string) to plain
  # atomic scalars, i.e. the text/numeric inputs this operator was written
  # for. Anything else - data.frames (legend schemes), lists, or vectors of
  # length > 1 - is returned as-is once we know it's non-NULL. Without this
  # guard, `rel_scheme %||% REL_AAL_DEFAULT_BINS` (a 9-row data.frame) would
  # run `is.na(a[1])` on a whole column, and R's `&&` errors out trying to
  # coerce that length-9 result down to a single logical.
  if (is.atomic(a) && length(a) == 1) {
    if (is.na(a) || (is.character(a) && !nzchar(a))) return(b)
  }
  a
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
# Legend Configuration Manager UI (Problems 2, 3, 4, 5, 6, 7)
#
# `editable = TRUE` renders the full editor: Load Default / Load JSON /
# Download JSON, Number of Bins + Create Bins, the editable bin table, and
# a live legend preview. `editable = FALSE` renders a compact read-only
# preview only - used on the Statewise tab, which shares the Relative AAL
# legend configured on the Regionwise tab instead of duplicating an
# editable copy of it.
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

      div(
        class = "vul-legend-toolbar-group",
        div(class = "vul-legend-toolbar-label", "Import / Export"),
        div(
          class = "vul-legend-toolbar-row",
          actionButton(ns(paste0(prefix, "_legend_load_default")), "Load Default", icon = icon("rotate-left"), class = "sec2-btn"),
          fileInput(ns(paste0(prefix, "_legend_json_file")), NULL, accept = ".json", buttonLabel = "Load JSON", placeholder = "No file", width = "230px"),
          downloadButton(ns(paste0(prefix, "_legend_download_json")), "Download JSON", class = "sec2-btn")
        )
      ),

      div(
        class = "vul-legend-toolbar-group",
        div(class = "vul-legend-toolbar-label", "Auto-generate"),
        div(
          class = "vul-legend-toolbar-row",
          numericInput(ns(paste0(prefix, "_legend_n_bins")), "Number of Bins", value = 9, min = 2, max = 15, step = 1, width = "140px"),
          actionButton(ns(paste0(prefix, "_legend_create_bins")), "Create Bins from Data", icon = icon("wand-magic-sparkles"), class = "sec2-btn")
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
      )
    )
  )
}

# ---- Legend preview swatches (shared renderer used by server.R) ----
# Rendered as a horizontal, wrapping row of chips (rather than a stacked
# list) so the full legend can be scanned at a glance. Each chip uses the
# same rounded-swatch look as the colour-picker inputs elsewhere in the
# app for visual consistency.
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

# ---- Black or white text for readability on top of a given hex fill ----
vul_contrast_text <- function(hex) {
  hex <- sub("^#", "", ifelse(is.na(hex) | !nzchar(hex), "FFFFFF", hex))
  hex <- ifelse(nchar(hex) == 6, hex, "FFFFFF")
  r <- strtoi(substr(hex, 1, 2), base = 16L)
  g <- strtoi(substr(hex, 3, 4), base = 16L)
  b <- strtoi(substr(hex, 5, 6), base = 16L)
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  ifelse(is.na(lum), "#1c2534", ifelse(lum > 0.55, "#1c2534", "#ffffff"))
}

# ---- Editable legend DT (shared renderer used by server.R) ----
# The Colour column's own cell is painted with the hex value it holds (and
# its text switched to black/white for contrast) so a typed-in colour is
# visible immediately, without needing a separate swatch look-up.
vul_legend_datatable <- function(scheme_df) {
  df <- scheme_df[order(-suppressWarnings(as.numeric(scheme_df$level))), c("level", "label", "lower", "upper", "colour")]
  names(df) <- c("Level", "Label", "Lower", "Upper", "Colour")
  dt <- DT::datatable(
    df,
    editable = list(target = "cell", disable = list(columns = c(0))),
    rownames = FALSE,
    selection = "none",
    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
  )
  uniq_cols <- unique(df$Colour)
  DT::formatStyle(
    dt, "Colour",
    backgroundColor = DT::styleEqual(uniq_cols, uniq_cols),
    color = DT::styleEqual(uniq_cols, vul_contrast_text(uniq_cols)),
    fontWeight = "600",
    textAlign = "center"
  )
}

# -----------------------------------------------------------------------------
# Gallery controls bar (collapsible, "Apply to all" button)
# -----------------------------------------------------------------------------
vul_gallery_controls_ui <- function(
    ns,
    prefix,
    n,
    default_export_w,
    default_export_h,
    default_dpi,
    default_display_w_pct = 100,
    default_display_h_px = 700,
    default_card_max_width_px = 900,
    default_axis_text = 12,
    default_axis_title = 14,
    default_plot_title = 16,
    default_strip_text = 12,
    default_strip_face = "bold",
    default_legend_text = 10,
    default_legend_title = 10,
    default_legend_pos = "top",
    default_legend_key_size = 0.8,
    default_show_labels = TRUE,
    default_data_label_size = 3.5,
    default_data_label_colour = "white",
    default_data_label_face = "bold",
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

        sec_control_section(
          "Output / Canvas Properties",
          numericInput(ns(paste0(prefix, "_default_export_w")), "Export Width (in)", value = default_export_w, min = 3, max = 20, step = 0.5, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_export_h")), "Export Height (in)", value = default_export_h, min = 2, max = 15, step = 0.5, width = "100px"),
          numericInput(ns(paste0(prefix, "_default_dpi")), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_display_w_pct")), "Heatmap Display Width (%)", value = default_display_w_pct, min = 20, max = 100, step = 5, width = "120px"),
          numericInput(ns(paste0(prefix, "_default_display_h_px")), "Heatmap Display Height (px)", value = default_display_h_px, min = 300, max = 2200, step = 20, width = "130px"),
          numericInput(ns(paste0(prefix, "_default_card_max_width_px")), "Plot Card Max Width (px)", value = default_card_max_width_px, min = 300, max = 2400, step = 20, width = "120px")
        ),

        sec_control_section(
          "Axis / Title / Strip Text",
          numericInput(ns(paste0(prefix, "_default_axis_text")), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_title")), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_plot_title")), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
          if (has_facets) numericInput(ns(paste0(prefix, "_default_strip_text")), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
          if (has_facets) selectInput(ns(paste0(prefix, "_default_strip_face")), "Strip font face", choices = c("plain", "bold", "italic", "bold.italic"), selected = default_strip_face, width = "120px")
        ),

        sec_control_section(
          "Legend Properties",
          selectInput(ns(paste0(prefix, "_default_legend_pos")), "Legend", choices = c("top", "bottom", "left", "right", "none"), selected = default_legend_pos, width = "80px"),
          checkboxInput(ns(paste0(prefix, "_legend_show")), "Show legend", value = TRUE),
          numericInput(ns(paste0(prefix, "_default_legend_text")), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_title")), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_key_size")), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
        ),

        sec_control_section(
          "Data Label Properties",
          checkboxInput(ns(paste0(prefix, "_default_show_labels")), "Show value labels", value = default_show_labels),
          numericInput(ns(paste0(prefix, "_default_data_label_size")), "Data label size", value = default_data_label_size, min = 1, max = 10, step = 0.5, width = "100px"),
          colourInput(ns(paste0(prefix, "_default_data_label_colour")), "Data label colour", value = default_data_label_colour, showColour = "text", width = "100px"),
          selectInput(ns(paste0(prefix, "_default_data_label_face")), "Data label font face", choices = c("plain", "bold", "italic", "bold.italic"), selected = default_data_label_face, width = "120px")
        ),

        if (!is.null(legend_ui)) legend_ui
      ),

      div(
        class = "sec-gallery-bar-foot",
        actionButton(ns(paste0(prefix, "_apply_all")), paste0("Apply to all ", n, " plots"), icon = icon("wand-magic-sparkles"), class = "sec2-btn")
      )
    )
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
    default_export_w = 9,
    default_export_h = 5,
    default_dpi = 150,
    default_display_w_pct = 100,
    default_display_h_px = 700,
    default_card_max_width_px = 900,
    default_axis_text = 12,
    default_axis_title = 14,
    default_plot_title = 16,
    default_strip_text = 12,
    default_strip_face = "bold",
    default_legend_text = 10,
    default_legend_title = 10,
    default_legend_key_size = 0.8,
    default_show_labels = TRUE,
    default_data_label_size = 3.5,
    default_data_label_colour = "white",
    default_data_label_face = "bold",
    has_facets = TRUE
) {

  ov_id <- ns(paste0(prefix, "_override_", key))

  div(
    class = "cart-item-card vul-plot-card",
    style = "margin-bottom:14px;",

    div(
      class = "cart-item-header",
      span(class = "cart-item-badge", label),
      div(
        style = "display:flex; gap:4px;",
        tags$button(
          onclick = sprintf("document.getElementById('%s').classList.toggle('sec-open');", ov_id),
          class = "btn-icon-cart sec2-icon-btn", title = "Adjust size, text & legend",
          icon("sliders-h")
        ),
        downloadButton(
          outputId = ns(paste0(prefix, "_dl_", key)), label = NULL, icon = icon("download"),
          class = "btn-icon-cart sec2-icon-btn", title = "Download"
        )
      )
    ),

    div(class = "sec-plot-frame", uiOutput(ns(paste0(prefix, "_plot_frame_", key)))),

    div(
      id = ov_id,
      class = "sec-override-panel",

      sec_control_section(
        "Output / Canvas Properties",
        numericInput(ns(paste0(prefix, "_export_w_", key)), "Export Width (in)", value = default_export_w, min = 3, max = 20, step = 0.5, width = "100px"),
        numericInput(ns(paste0(prefix, "_export_h_", key)), "Export Height (in)", value = default_export_h, min = 2, max = 15, step = 0.5, width = "100px"),
        numericInput(ns(paste0(prefix, "_dpi_", key)), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
        numericInput(ns(paste0(prefix, "_display_w_pct_", key)), "Plot Card Width (%)", value = default_display_w_pct, min = 20, max = 100, step = 5, width = "110px"),
        numericInput(ns(paste0(prefix, "_display_h_px_", key)), "Plot Card Height (px)", value = default_display_h_px, min = 300, max = 2200, step = 20, width = "120px"),
        numericInput(ns(paste0(prefix, "_card_max_width_px_", key)), "Plot Card Max Width (px)", value = default_card_max_width_px, min = 300, max = 2400, step = 20, width = "120px")
      ),

      sec_control_section(
        "Axis / Title / Strip Text",
        numericInput(ns(paste0(prefix, "_axis_text_", key)), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_title_", key)), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_title_", key)), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
        if (has_facets) numericInput(ns(paste0(prefix, "_strip_text_", key)), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px"),
        if (has_facets) selectInput(ns(paste0(prefix, "_strip_face_", key)), "Strip font face", choices = c("plain", "bold", "italic", "bold.italic"), selected = default_strip_face, width = "120px")
      ),

      sec_control_section(
        "Legend Properties",
        selectInput(ns(paste0(prefix, "_legend_pos_", key)), "Legend", choices = c("top", "bottom", "left", "right", "none"), selected = "top", width = "80px"),
        checkboxInput(ns(paste0(prefix, "_legend_show_", key)), "Show legend", value = TRUE),
        numericInput(ns(paste0(prefix, "_legend_text_", key)), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_title_", key)), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_key_size_", key)), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
      ),

      sec_control_section(
        "Data Label Properties",
        checkboxInput(ns(paste0(prefix, "_show_labels_", key)), "Show value labels", value = default_show_labels),
        numericInput(ns(paste0(prefix, "_data_label_size_", key)), "Data label size", value = default_data_label_size, min = 1, max = 10, step = 0.5, width = "100px"),
        colourInput(ns(paste0(prefix, "_data_label_colour_", key)), "Data label colour", value = default_data_label_colour, showColour = "text", width = "100px"),
        selectInput(ns(paste0(prefix, "_data_label_face_", key)), "Data label font face", choices = c("plain", "bold", "italic", "bold.italic"), selected = default_data_label_face, width = "120px")
      )
    )
  )
}
