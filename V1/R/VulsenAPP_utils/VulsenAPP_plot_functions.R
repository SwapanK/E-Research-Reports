# =============================================================================
# VulsenAPP_plot_functions.R – all plotting functions for VulSen heatmaps
# Extended with override application and bundle creation
# =============================================================================

# ---- Apply overrides to a ggplot (adapted from Secmod) ----
vul_apply_overrides <- function(p, input, prefix, key, defaults) {
  # Read all override values with fallback to defaults
  axis_text <- input[[paste0(prefix, "_axis_text_", key)]] %||% defaults$axis_text %||% 12
  axis_title <- input[[paste0(prefix, "_axis_title_", key)]] %||% defaults$axis_title %||% 14
  plot_title <- input[[paste0(prefix, "_plot_title_", key)]] %||% defaults$plot_title %||% 16
  strip_text <- input[[paste0(prefix, "_strip_text_", key)]] %||% defaults$strip_text %||% 12
  legend_text <- input[[paste0(prefix, "_legend_text_", key)]] %||% defaults$legend_text %||% 10
  legend_title <- input[[paste0(prefix, "_legend_title_", key)]] %||% defaults$legend_title %||% 10
  axis_angle <- input[[paste0(prefix, "_axis_angle_", key)]] %||% defaults$axis_angle %||% 90
  legend_pos <- input[[paste0(prefix, "_legend_pos_", key)]] %||% defaults$legend_pos %||% "top"
  show_legend <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size %||% 0.8
  title_hjust <- input[[paste0(prefix, "_plot_title_hjust_", key)]] %||% defaults$title_hjust %||% 0.5
  axis_line_col <- input[[paste0(prefix, "_axis_line_col_", key)]] %||% defaults$axis_line_col %||% "black"
  panel_fill <- input[[paste0(prefix, "_panel_fill_", key)]] %||% defaults$panel_fill %||% "white"
  bg_choice <- input[[paste0(prefix, "_bg_", key)]] %||% defaults$bg %||% "white"
  panel_spacing <- input[[paste0(prefix, "_panel_spacing_", key)]] %||% defaults$panel_spacing %||% 3
  margin_t <- input[[paste0(prefix, "_plot_margin_t_", key)]] %||% defaults$margin_t %||% 30
  margin_r <- input[[paste0(prefix, "_plot_margin_r_", key)]] %||% defaults$margin_r %||% 10
  margin_b <- input[[paste0(prefix, "_plot_margin_b_", key)]] %||% defaults$margin_b %||% 30
  margin_l <- input[[paste0(prefix, "_plot_margin_l_", key)]] %||% defaults$margin_l %||% 10
  border_col <- input[[paste0(prefix, "_panel_border_col_", key)]] %||% defaults$border_col %||% "black"
  border_lwd <- input[[paste0(prefix, "_panel_border_lwd_", key)]] %||% defaults$border_lwd %||% 0.5
  axis_text_margin_t <- input[[paste0(prefix, "_axis_text_margin_t_", key)]] %||% defaults$axis_text_margin_t %||% 5
  axis_text_vjust <- input[[paste0(prefix, "_axis_text_vjust_", key)]] %||% defaults$axis_text_vjust %||% 0.5
  
  # Tile-level controls. These heatmaps have no gridlines by design
  # (panel.grid is blanked in plot_rel_gg()/plot_pct_gg()) - the only
  # visible "grid" is the border drawn around each geom_tile() cell, so
  # that's what "grid_col" now controls instead of the invisible
  # panel.grid.major.
  tile_border_col <- input[[paste0(prefix, "_grid_col_", key)]] %||% defaults$grid_col %||% "white"
  tile_border_lwd <- input[[paste0(prefix, "_tile_border_lwd_", key)]] %||% defaults$tile_border_lwd %||% 0.22
  show_labels <- input[[paste0(prefix, "_show_labels_", key)]]
  if (is.null(show_labels)) show_labels <- defaults$show_labels %||% TRUE
  
  is_transp <- isTRUE(tolower(trimws(as.character(panel_fill))) %in% c("transparent", "na", "none")) ||
    isTRUE(tolower(trimws(as.character(bg_choice))) %in% c("transparent", "na", "none"))
  
  # Handle dense facets if many panels
  n_panels <- tryCatch({
    built <- ggplot2::ggplot_build(p)
    nrow(built$layout$layout)
  }, error = function(e) 1L)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  
  panel_spacing <- panel_spacing * dense_scale
  margin_t <- margin_t * dense_scale
  margin_r <- margin_r * dense_scale
  margin_b <- margin_b * dense_scale
  margin_l <- margin_l * dense_scale
  axis_text <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))
  
  p <- p + theme(
    axis.text.x = element_text(
      size = axis_text,
      angle = axis_angle,
      hjust = 1,
      vjust = axis_text_vjust,
      margin = margin(t = axis_text_margin_t)
    ),
    axis.text.y = element_text(size = axis_text),
    axis.title.x = element_text(size = axis_title),
    axis.title.y = element_text(size = axis_title),
    plot.title = element_text(size = plot_title, hjust = title_hjust),
    strip.text = element_text(size = strip_text),
    strip.background = element_blank(),
    legend.text = element_text(size = legend_text),
    legend.title = element_text(size = legend_title),
    legend.position = if (show_legend) legend_pos else "none",
    legend.key.size = unit(legend_key_size, "cm"),
    axis.line = element_line(colour = axis_line_col),
    axis.ticks = element_line(colour = axis_line_col),
    panel.background  = element_rect(fill = if (is_transp) NA else panel_fill, colour = NA),
    plot.background   = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.background = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    legend.key        = element_rect(fill = if (is_transp) NA else "white", colour = NA),
    panel.spacing = unit(panel_spacing, "lines"),
    plot.margin = margin(t = margin_t, r = margin_r, b = margin_b, l = margin_l),
    panel.border = element_rect(colour = border_col, fill = NA, linewidth = border_lwd)
  )
  
  # Tile border colour/width - the geom_tile() layer's own border,
  # not a gridline. Fill colour (GAL_COLORS/PCT_COLORS) is left
  # completely untouched here on purpose.
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomTile")) {
      p$layers[[i]]$aes_params$colour <- tile_border_col
      p$layers[[i]]$aes_params$linewidth <- tile_border_lwd
    }
  }
  
  # Show/hide the value label printed inside each tile. Size stays on
  # the plot's own dynamic sizing (compute_dynamic_sizes()) so it keeps
  # adapting to grid density; this control only toggles visibility.
  if (!isTRUE(show_labels)) {
    is_text_layer <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
    p$layers <- p$layers[!is_text_layer]
  }
  
  p <- p + coord_cartesian(clip = "off")
  attr(p, "vulsen_bg") <- if (is_transp) "transparent" else "white"
  p
}

# ---- Preprocessing for heatmap data ----
prep_heatmap_df <- function(df, yvar, class_label, model_1, model_2) {
  d <- as.data.frame(df)
  d <- label_classes(d)
  if (class_label %in% names(ORDER_LIST)) {
    d$Description <- factor(as.character(d$Description), levels = ORDER_LIST[[class_label]])
  }
  if (yvar == "Region") {
    d[[yvar]] <- factor(as.character(d[[yvar]]), levels = REGION_ORDER[REGION_ORDER %in% unique(as.character(d[[yvar]]))])
  }
  d$variable <- dplyr::recode(d$variable, "New" = model_1, "Old" = model_2, .default = d$variable)
  d
}

# ---- Dynamic sizing ----
compute_dynamic_sizes <- function(nx, ny, scope = c("region", "state", "pct")) {
  scope <- match.arg(scope)
  base_area <- max(nx * ny, 1)
  matrix_label <- if (scope == "state") {
    max(2.1, min(4.4, 95 / sqrt(base_area)))
  } else {
    max(2.5, min(5.2, 110 / sqrt(base_area)))
  }
  axis_x <- max(7.3, min(13.0, 95 / max(nx, 1)))
  axis_y <- max(7.3, min(13.0, 150 / max(ny, 1)))
  list(matrix_label = matrix_label, axis_x = axis_x, axis_y = axis_y)
}

# ---- Add relative bins ----
add_rel_bin <- function(d, breaks, intervals) {
  cutv <- cut(d$value, breaks = breaks, include.lowest = TRUE)
  lev <- levels(cutv)
  map <- data.frame(Relative_ALL = lev, Mapping = intervals[seq_along(lev)], stringsAsFactors = FALSE)
  d$Relative_ALL <- factor(map$Mapping[match(as.character(cutv), map$Relative_ALL)], levels = map$Mapping)
  d$fill_col <- GAL_COLORS[match(d$Relative_ALL, map$Mapping)]
  d$hover <- paste0(
    "Primary Modifier: ", d$Description,
    "<br>", ifelse(names(d)[names(d) %in% c("Region", "STATECODE")][1] == "Region", "Region", "State"),
    ": ", d[[names(d)[names(d) %in% c("Region", "STATECODE")][1]]],
    "<br>Version: ", d$variable,
    "<br>Relative AAL: ", sprintf("%.3f", d$value)
  )
  d$value_lab <- ifelse(is.na(d$value), "", sprintf("%.2f", d$value))
  d
}

# ---- Relative AAL heatmap (ggplot) ----
plot_rel_gg <- function(
    df,
    yvar,
    class_label,
    title,
    breaks,
    intervals,
    model_1,
    model_2,
    scope = c("region", "state"),
    side_by_side = FALSE
) {
  scope <- match.arg(scope)
  d <- prep_heatmap_df(df, yvar, class_label, model_1, model_2)
  d <- add_rel_bin(d, breaks, intervals)
  
  nx <- length(unique(d$Description))
  ny <- length(unique(d[[yvar]]))
  sz <- compute_dynamic_sizes(nx, ny, scope)
  
  ttl <- if (isTRUE(side_by_side)) {
    paste0(title, " - side-by-side focus")
  } else {
    title
  }
  
  tile_ratio <- if (nx <= 3) 0.42 else if (nx <= 5) 0.65 else if (nx <= 7) 0.82 else 1
  
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = Description,
      y = .data[[yvar]],
      fill = Relative_ALL,
      text = hover
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.22) +
    ggplot2::facet_wrap(~variable, ncol = 2) +
    ggplot2::geom_text(
      ggplot2::aes(label = value_lab),
      color = "white",
      size = sz$matrix_label,
      fontface = "bold",
      na.rm = TRUE
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = c(0.001, 0.001)),
      guide = ggplot2::guide_axis(n.dodge = ifelse(nx > 7, 2, 1))
    ) +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(mult = c(0.001, 0.001)),
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = GAL_COLORS[seq_along(levels(d$Relative_ALL))],
      limits = levels(d$Relative_ALL),
      drop = FALSE
    ) +
    ggplot2::coord_fixed(ratio = tile_ratio, clip = "off") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = TRUE,
        nrow = 1,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(title = ttl, fill = "Relative AAL") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        color = "#002B49",
        size = ifelse(scope == "state", 19, 23),
        margin = ggplot2::margin(t = 0, b = 10)
      ),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = ifelse(scope == "state", 12, 16),
        margin = ggplot2::margin(t = 2, b = 1)
      ),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10.2),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = sz$axis_x,
        angle = 0,
        hjust = 0.5,
        face = "bold",
        margin = ggplot2::margin(t = 1)
      ),
      axis.text.y = ggplot2::element_text(
        size = sz$axis_y,
        face = "bold",
        margin = ggplot2::margin(r = 1)
      ),
      plot.margin = grid::unit(c(0.02, 0.04, 0.02, 0.01), "in"),
      panel.spacing = grid::unit(0.22, "lines")
    )
}

# ---- Percentage change data preparation ----
make_pct_data <- function(obj) {
  raw <- obj$region_split
  out <- lapply(names(raw), function(nm) {
    d <- raw[[nm]] |>
      dplyr::select(Description, Classification, Region, variable, value) |>
      tidyr::pivot_wider(names_from = variable, values_from = value) |>
      dplyr::mutate(value = (New / Old - 1) * 100)
    d$Classification <- nm
    d
  })
  names(out) <- names(raw)
  out
}

# ---- Add percentage bins ----
add_pct_bin <- function(d) {
  cutv <- cut(d$value, breaks = PCT_BREAKS, include.lowest = TRUE)
  lev <- levels(cutv)
  map <- data.frame(Relative_ALL = lev, Mapping = PCT_INTERVALS[seq_along(lev)], stringsAsFactors = FALSE)
  d$Relative_ALL <- factor(map$Mapping[match(as.character(cutv), map$Relative_ALL)], levels = map$Mapping)
  d$hover <- paste0(
    "Primary Modifier: ", d$Description,
    "<br>Region: ", d$Region,
    "<br>% change: ", sprintf("%.2f", d$value)
  )
  d$value_lab <- ifelse(is.na(d$value), "", sprintf("%.1f", d$value))
  d
}

# ---- Percentage change heatmap (ggplot) ----
plot_pct_gg <- function(d, class_label) {
  d$Classification <- class_label
  
  if (class_label %in% names(ORDER_LIST)) {
    d$Description <- factor(as.character(d$Description), levels = ORDER_LIST[[class_label]])
  }
  
  d$Region <- factor(
    as.character(d$Region),
    levels = REGION_ORDER[REGION_ORDER %in% unique(as.character(d$Region))]
  )
  
  d <- add_pct_bin(d)
  
  nx <- length(unique(d$Description))
  ny <- length(unique(d$Region))
  sz <- compute_dynamic_sizes(nx, ny, "pct")
  
  tile_ratio <- if (nx <= 3) 0.42 else if (nx <= 5) 0.65 else if (nx <= 7) 0.82 else 1
  
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = Description,
      y = Region,
      fill = Relative_ALL,
      text = hover
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.22) +
    ggplot2::geom_text(
      ggplot2::aes(label = value_lab),
      color = "white",
      size = sz$matrix_label,
      fontface = "bold",
      na.rm = TRUE
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = c(0.001, 0.001)),
      guide = ggplot2::guide_axis(n.dodge = ifelse(nx > 7, 2, 1))
    ) +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(mult = c(0.001, 0.001)),
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = PCT_COLORS[seq_along(levels(d$Relative_ALL))],
      limits = levels(d$Relative_ALL),
      drop = FALSE
    ) +
    ggplot2::coord_fixed(ratio = tile_ratio, clip = "off") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)
    ) +
    ggplot2::labs(title = class_label, fill = "% change") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        color = "#002B49",
        size = 23,
        margin = ggplot2::margin(t = 0, b = 10)
      ),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10.2),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = sz$axis_x,
        angle = 0,
        hjust = 0.5,
        face = "bold",
        margin = ggplot2::margin(t = 1)
      ),
      axis.text.y = ggplot2::element_text(
        size = sz$axis_y,
        face = "bold",
        margin = ggplot2::margin(r = 1)
      ),
      plot.margin = grid::unit(c(0.02, 0.04, 0.02, 0.01), "in")
    )
}

# ---- GG to Plotly ----
gg_to_plotly <- function(p) {
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      legend = list(
        orientation = "h",
        y = 0.98,
        x = 0.5,
        xanchor = "center"
      ),
      margin = list(
        l = 45,
        r = 20,
        b = 45,
        t = 95
      )
    )
}

# ---- Get plot dimensions for UI ----
plot_dims <- function(data, scope = c("region", "state", "pct")) {
  scope <- match.arg(scope)
  yvar <- if (scope == "state") "STATECODE" else "Region"
  n_y <- length(unique(data[[yvar]]))
  n_x <- length(unique(data$Description))
  
  base_h <- plot_height_for(scope, n_y, n_x)
  if (n_x <= 3 && scope %in% c("region", "pct")) {
    base_h <- "700px"
  }
  if (n_x <= 3 && scope == "state") {
    base_h <- "850px"
  }
  list(height = base_h, n_y = n_y, n_x = n_x)
}

# ---- Build all static plots ----
build_all_static_gplots <- function(obj, side_by_side = FALSE) {
  region <- lapply(names(obj$region_split), function(nm) {
    bi <- obj$region_breaks[[nm]]
    plot_rel_gg(
      obj$region_split[[nm]],
      "Region",
      nm,
      obj$model_title,
      bi$breaks,
      bi$intervals,
      obj$model_1,
      obj$model_2,
      "region",
      side_by_side
    )
  })
  names(region) <- names(obj$region_split)
  
  state <- lapply(names(obj$state_split), function(nm) {
    bi <- obj$state_breaks[[nm]]
    plot_rel_gg(
      obj$state_split[[nm]],
      "STATECODE",
      nm,
      obj$model_title,
      bi$breaks,
      bi$intervals,
      obj$model_1,
      obj$model_2,
      "state",
      side_by_side
    )
  })
  names(state) <- names(obj$state_split)
  
  pct_data <- make_pct_data(obj)
  pct <- lapply(names(pct_data), function(nm) plot_pct_gg(pct_data[[nm]], nm))
  names(pct) <- names(pct_data)
  
  list(
    region = region,
    state = state,
    pct = pct,
    pct_data = pct_data
  )
}

# ---- Build plot bundles (list of plot objects with default overrides) ----
vul_make_plot_bundles <- function(plots, default_vals) {
  # 'plots' is a list of plot lists: region, state, pct
  # Returns a nested list: list(region = list(key = list(plot = <ggplot>, ...)), ...)
  bundles <- list()
  for (group in names(plots)) {
    if (group == "pct_data") next
    group_plots <- plots[[group]]
    group_bundles <- list()
    for (nm in names(group_plots)) {
      group_bundles[[nm]] <- list(
        plot = group_plots[[nm]],
        width = default_vals$w %||% 9,
        height = default_vals$h %||% 5,
        dpi = default_vals$dpi %||% 150,
        bg = default_vals$bg %||% "white"
      )
    }
    bundles[[group]] <- group_bundles
  }
  bundles
}



