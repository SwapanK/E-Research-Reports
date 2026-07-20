# =============================================================================
# VulsenAPP_ui_helpers.R – UI helpers for VulSen app
# Extended with lightbulb hints, gallery controls, and plot cards
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(as.character(a[1]))) a else b

# -----------------------------------------------------------------------------
# Header / branding (used in standalone HTML reports)
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
        "responsive heatmaps, hover tooltips, side‑by‑side comparison toggle, and HTML export."
      ),
      if (!is.null(subtitle)) htmltools::tags$p(subtitle)
    )
  )
}

# -----------------------------------------------------------------------------
# Metric card (for summary stats)
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
  cls <- switch(type,
                warning = "warning-box",
                error   = "error-box",
                success = "success-box"
  )
  htmltools::tags$div(class = cls, text)
}

# -----------------------------------------------------------------------------
# Plot card (simple wrapper for plotlyOutput) – NOW WITH NAMESPACE SUPPORT
# -----------------------------------------------------------------------------
plot_card <- function(output_id, title, caption = NULL, height = "900px", ns = identity) {
  htmltools::tags$div(
    class = "plot-card",
    htmltools::tags$h3(title),
    plotly::plotlyOutput(ns(output_id), height = height),
    if (!is.null(caption) && nzchar(caption)) {
      htmltools::tags$div(class = "plot-caption", caption)
    }
  )
}

# -----------------------------------------------------------------------------
# Dynamic plot height based on number of rows/columns
# -----------------------------------------------------------------------------
plot_height_for <- function(scope = c("region", "state", "pct"), n_y = 8, n_x = 8) {
  scope <- match.arg(scope)
  if (scope == "state") {
    return(paste0(max(960, min(1800, 300 + 21 * n_y + 18 * n_x)), "px"))
  }
  if (scope == "pct") {
    return(paste0(max(760, min(1180, 280 + 20 * n_y + 16 * n_x)), "px"))
  }
  paste0(max(760, min(1180, 320 + 18 * n_y + 14 * n_x)), "px")
}

# -----------------------------------------------------------------------------
# Lightbulb hint (shared with MetaApp)
# -----------------------------------------------------------------------------
sec_hint <- function(text) {
  div(
    class = "sec2-hint",
    span(class = "sec2-hint-icon", icon("lightbulb")),
    span(text)
  )
}

# -----------------------------------------------------------------------------
# Gallery controls bar (collapsible, "Apply to all" button)
# -----------------------------------------------------------------------------
vul_gallery_controls_ui <- function(
    ns,
    prefix,               # e.g. "region", "state", "pct"
    n,                    # number of plots in this group
    default_w,
    default_h,
    default_dpi,
    default_axis_text = 12,
    default_axis_title = 14,
    default_plot_title = 16,
    default_strip_text = 12,
    default_legend_text = 10,
    default_legend_title = 10,
    default_axis_angle = 90,
    default_legend_pos = "top",
    default_col_sfd = "#6FACDE",
    default_col_com = "#F0B323",
    default_col_pen = "#F0B323",
    default_col_cred = "#6FACDE",
    default_legend_key_size = 0.8,
    default_title_hjust = 0.5,
    default_axis_line_col = "black",
    default_panel_fill = "white",
    default_grid_col = "#e9ecf3",
    default_panel_spacing = 3,
    default_margin_t = 30,
    default_margin_r = 10,
    default_margin_b = 30,
    default_margin_l = 10,
    default_border_col = "black",
    default_border_lwd = 0.5,
    default_bg = "white",
    default_axis_text_margin_t = 5,
    default_axis_text_vjust = 0.5
) {
  
  div(
    class = "sec-gallery-bar sec-gallery-collapsed", # default collapsed
    div(
      class = "sec-gallery-bar-head",
      div(
        class = "sec-gallery-bar-title",
        icon("sliders-h"),
        "Gallery defaults"
      ),
      div(
        style = "display:flex; align-items:center; gap:8px;",
        span(class = "sec-gallery-bar-count", paste(n, "plots")),
        tags$button(
          class = "sec-gallery-toggle",
          onclick = "toggleGallery(this)",
          icon("chevron-down")
        )
      )
    ),
    # ---- Body (collapsible) ----
    div(
      class = "sec-gallery-body",
      sec_hint("Changes take effect after clicking ‘Apply to all’ below."),
      
      # ---- Grouped controls ----
      div(
        class = "sec-gallery-controls",
        
        # Output / Canvas
        sec_control_section(
          "Output / Canvas Properties",
          numericInput(ns(paste0(prefix, "_default_w")), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_h")), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_dpi")), "DPI", value = default_dpi, min = 72, max = 300, step = 10, width = "80px"),
          selectInput(ns(paste0(prefix, "_default_bg")), "Background", choices = c("White" = "white", "Transparent" = "transparent"), selected = default_bg, width = "100px")
        ),
        
        # Axis Properties
        sec_control_section(
          "Axis Properties",
          numericInput(ns(paste0(prefix, "_default_axis_text")), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_title")), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_angle")), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_text_margin_t")), "X label gap", value = default_axis_text_margin_t, min = 0, max = 50, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_axis_text_vjust")), "X label vjust", value = default_axis_text_vjust, min = 0, max = 1, step = 0.05, width = "80px"),
          colourInput(ns(paste0(prefix, "_default_axis_line_col")), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px")
        ),
        
        # Title / Facet Text
        sec_control_section(
          "Title / Facet Text Properties",
          numericInput(ns(paste0(prefix, "_default_plot_title")), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_title_hjust")), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_strip_text")), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
        ),
        
        # Legend Properties
        sec_control_section(
          "Legend Properties",
          selectInput(ns(paste0(prefix, "_default_legend_pos")), "Legend", choices = c("top","bottom","left","right","none"), selected = default_legend_pos, width = "80px"),
          checkboxInput(ns(paste0(prefix, "_legend_show")), "Show legend", value = TRUE),
          numericInput(ns(paste0(prefix, "_default_legend_text")), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_title")), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_legend_key_size")), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
        ),
        
        # Panel / Spacing / Margin
        sec_control_section(
          "Panel / Spacing / Margin Properties",
          numericInput(ns(paste0(prefix, "_default_panel_spacing")), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_margin_t")), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_margin_r")), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_margin_b")), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_margin_l")), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
          colourInput(ns(paste0(prefix, "_default_panel_fill")), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
          colourInput(ns(paste0(prefix, "_default_grid_col")), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
          colourInput(ns(paste0(prefix, "_default_border_col")), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
          numericInput(ns(paste0(prefix, "_default_border_lwd")), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px")
        ),
        
        # Color Properties
        sec_control_section(
          "Color Properties",
          colourInput(ns(paste0(prefix, "_default_col_sfd")), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
          colourInput(ns(paste0(prefix, "_default_col_com")), "COM", value = default_col_com, showColour = "text", width = "80px"),
          colourInput(ns(paste0(prefix, "_default_col_pen")), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
          colourInput(ns(paste0(prefix, "_default_col_cred")), "Credit", value = default_col_cred, showColour = "text", width = "80px")
        ),
        
        # Data Label Properties
        sec_control_section(
          "Data Label Properties",
          numericInput(ns(paste0(prefix, "_default_label_size")), "Label size", value = 3, min = 2, max = 10, step = 0.2, width = "80px"),
          numericInput(ns(paste0(prefix, "_default_label_angle")), "Label angle", value = 0, min = 0, max = 90, step = 5, width = "80px")
        )
      ),
      
      # ---- Footer with "Apply to all" ----
      div(
        class = "sec-gallery-bar-foot",
        actionButton(
          ns(paste0(prefix, "_apply_all")),
          paste0("Apply to all ", n, " plots"),
          icon = icon("wand-magic-sparkles"),
          class = "sec2-btn"
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# sec_control_section – helper for grouped controls
# -----------------------------------------------------------------------------
sec_control_section <- function(title, ..., section_class = "sec2-control-section") {
  div(
    class = section_class,
    div(class = "sec2-control-section-title", title),
    div(class = "sec2-control-section-grid", ...)
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
    default_w = 9,
    default_h = 5,
    default_axis_text = 12,
    default_axis_title = 14,
    default_plot_title = 16,
    default_strip_text = 12,
    default_legend_text = 10,
    default_legend_title = 10,
    default_axis_angle = 90,
    default_legend_key_size = 0.8,
    default_title_hjust = 0.5,
    default_panel_spacing = 3,
    default_margin_t = 30,
    default_margin_r = 10,
    default_margin_b = 30,
    default_margin_l = 10,
    default_border_lwd = 0.5,
    default_axis_line_col = "black",
    default_panel_fill = "white",
    default_grid_col = "#e9ecf3",
    default_border_col = "black",
    default_col_sfd = "#6FACDE",
    default_col_com = "#F0B323",
    default_col_pen = "#F0B323",
    default_col_cred = "#6FACDE",
    default_axis_text_margin_t = 5,
    default_axis_text_vjust = 0.5
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
          onclick = sprintf("$('#%s').toggleClass('sec-open');", ov_id),
          class = "btn-icon-cart sec2-icon-btn",
          title = "Adjust size, colours, text & legend",
          icon("sliders-h")
        ),
        downloadButton(
          outputId = ns(paste0(prefix, "_dl_", key)),
          label = NULL,
          icon = icon("download"),
          class = "btn-icon-cart sec2-icon-btn",
          title = "Download"
        )
      )
    ),
    
    div(
      class = "sec-plot-frame",
      uiOutput(ns(paste0(prefix, "_plot_frame_", key)))
    ),
    
    div(
      id = ov_id,
      class = "sec-override-panel",
      
      # ---- Output / Canvas Properties ----
      sec_control_section(
        "Output / Canvas Properties",
        numericInput(ns(paste0(prefix, "_w_", key)), "Width", value = default_w, min = 3, max = 20, step = 0.5, width = "80px"),
        numericInput(ns(paste0(prefix, "_h_", key)), "Height", value = default_h, min = 2, max = 15, step = 0.5, width = "80px"),
        numericInput(ns(paste0(prefix, "_dpi_", key)), "DPI", value = 150, min = 72, max = 300, step = 10, width = "80px"),
        selectInput(ns(paste0(prefix, "_bg_", key)), "Background",
                    choices = c("White" = "white", "Transparent" = "transparent"),
                    selected = "white", width = "100px")
      ),
      
      # ---- Axis Properties ----
      sec_control_section(
        "Axis Properties",
        numericInput(ns(paste0(prefix, "_axis_text_", key)), "Axis text", value = default_axis_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_title_", key)), "Axis title", value = default_axis_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_angle_", key)), "X angle", value = default_axis_angle, min = 0, max = 90, step = 5, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_text_margin_t_", key)), "X label gap", value = default_axis_text_margin_t, min = 0, max = 50, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_axis_text_vjust_", key)), "X label vjust", value = default_axis_text_vjust, min = 0, max = 1, step = 0.05, width = "80px"),
        colourInput(ns(paste0(prefix, "_axis_line_col_", key)), "Axis line", value = default_axis_line_col, showColour = "text", width = "80px")
      ),
      
      # ---- Title / Facet Text Properties ----
      sec_control_section(
        "Title / Facet Text Properties",
        numericInput(ns(paste0(prefix, "_plot_title_", key)), "Plot title", value = default_plot_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_title_hjust_", key)), "Title hjust", value = default_title_hjust, min = 0, max = 1, step = 0.05, width = "80px"),
        numericInput(ns(paste0(prefix, "_strip_text_", key)), "Strip text", value = default_strip_text, min = 6, max = 30, step = 1, width = "80px")
      ),
      
      # ---- Legend Properties ----
      sec_control_section(
        "Legend Properties",
        selectInput(ns(paste0(prefix, "_legend_pos_", key)), "Legend",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "top", width = "80px"),
        checkboxInput(ns(paste0(prefix, "_legend_show_", key)), "Show legend", value = TRUE),
        numericInput(ns(paste0(prefix, "_legend_text_", key)), "Legend text", value = default_legend_text, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_title_", key)), "Legend title", value = default_legend_title, min = 6, max = 30, step = 1, width = "80px"),
        numericInput(ns(paste0(prefix, "_legend_key_size_", key)), "Legend key", value = default_legend_key_size, min = 0.1, max = 3, step = 0.1, width = "80px")
      ),
      
      # ---- Panel / Spacing / Margin Properties ----
      sec_control_section(
        "Panel / Spacing / Margin Properties",
        numericInput(ns(paste0(prefix, "_panel_spacing_", key)), "Panel spacing", value = default_panel_spacing, min = 0, max = 10, step = 0.5, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_margin_t_", key)), "Margin top", value = default_margin_t, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_margin_r_", key)), "Margin right", value = default_margin_r, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_margin_b_", key)), "Margin bottom", value = default_margin_b, min = 0, max = 100, step = 5, width = "80px"),
        numericInput(ns(paste0(prefix, "_plot_margin_l_", key)), "Margin left", value = default_margin_l, min = 0, max = 100, step = 5, width = "80px"),
        colourInput(ns(paste0(prefix, "_panel_fill_", key)), "Panel bg", value = default_panel_fill, showColour = "text", width = "80px"),
        colourInput(ns(paste0(prefix, "_grid_col_", key)), "Grid colour", value = default_grid_col, showColour = "text", width = "80px"),
        colourInput(ns(paste0(prefix, "_panel_border_col_", key)), "Border colour", value = default_border_col, showColour = "text", width = "80px"),
        numericInput(ns(paste0(prefix, "_panel_border_lwd_", key)), "Border lwd", value = default_border_lwd, min = 0, max = 5, step = 0.1, width = "80px")
      ),
      
      # ---- Color Properties ----
      sec_control_section(
        "Color Properties",
        colourInput(ns(paste0(prefix, "_col_sfd_", key)), "SFD", value = default_col_sfd, showColour = "text", width = "80px"),
        colourInput(ns(paste0(prefix, "_col_com_", key)), "COM", value = default_col_com, showColour = "text", width = "80px"),
        colourInput(ns(paste0(prefix, "_col_pen_", key)), "Penalty", value = default_col_pen, showColour = "text", width = "80px"),
        colourInput(ns(paste0(prefix, "_col_cred_", key)), "Credit", value = default_col_cred, showColour = "text", width = "80px")
      ),
      
      # ---- Data Label Properties ----
      sec_control_section(
        "Data Label Properties",
        numericInput(ns(paste0(prefix, "_label_size_", key)), "Label size", value = 3, min = 2, max = 10, step = 0.2, width = "80px"),
        numericInput(ns(paste0(prefix, "_label_angle_", key)), "Label angle", value = 0, min = 0, max = 90, step = 5, width = "80px")
      )
    )
  )
}




