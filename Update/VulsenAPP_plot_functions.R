# =============================================================================
# VulsenAPP_plot_functions.R - all plotting functions for VulSen heatmaps
# Extended with scheme-driven legend fill and data-label/strip styling
# overrides.
#
# CHANGE LOG (this pass):
#   - Added resilience for missing Region column.
#   - No other changes to existing overrides.
# =============================================================================

# ---- Apply overrides to a ggplot ----
# (unchanged)
vul_apply_overrides <- function(p, input, prefix, key, defaults) {
  
  # ---- Text sizing ----
  axis_text    <- input[[paste0(prefix, "_axis_text_", key)]]    %||% defaults$axis_text    %||% 12
  plot_title   <- input[[paste0(prefix, "_plot_title_", key)]]   %||% defaults$plot_title   %||% 16
  strip_text   <- input[[paste0(prefix, "_strip_text_", key)]]   %||% defaults$strip_text   %||% 12
  legend_text  <- input[[paste0(prefix, "_legend_text_", key)]]  %||% defaults$legend_text  %||% 10
  legend_title <- input[[paste0(prefix, "_legend_title_", key)]] %||% defaults$legend_title %||% 10
  
  # ---- X-axis label rotation / vjust ----
  x_rotation <- input[[paste0(prefix, "_x_rotation_", key)]] %||% defaults$x_rotation %||% 0
  x_vjust    <- input[[paste0(prefix, "_x_vjust_", key)]]    %||% defaults$x_vjust    %||% 0.5
  x_rotation <- suppressWarnings(as.numeric(x_rotation)) %||% 0
  x_vjust    <- suppressWarnings(as.numeric(x_vjust))    %||% 0.5
  x_hjust <- if (x_rotation != 0) 1 else 0.5
  
  # ---- Legend visibility / key size ----
  show_legend <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size %||% 0.8
  
  # ---- Value label visibility / size / colour ----
  show_labels <- input[[paste0(prefix, "_show_labels_", key)]]
  if (is.null(show_labels)) show_labels <- defaults$show_labels %||% TRUE
  data_label_size   <- input[[paste0(prefix, "_data_label_size_", key)]]   %||% defaults$data_label_size   %||% 3.5
  data_label_colour <- input[[paste0(prefix, "_data_label_colour_", key)]] %||% defaults$data_label_colour %||% "#FFFFFF"
  
  # ---- Canvas / export properties ----
  width_in  <- input[[paste0(prefix, "_width_in_", key)]]  %||% defaults$width_in  %||% 9
  height_in <- input[[paste0(prefix, "_height_in_", key)]] %||% defaults$height_in %||% 5
  dpi       <- input[[paste0(prefix, "_dpi_", key)]]       %||% defaults$dpi       %||% 300
  panel_gap_px <- input[[paste0(prefix, "_panel_gap_px_", key)]] %||% defaults$panel_gap_px %||% 8
  panel_gap_px <- suppressWarnings(as.numeric(panel_gap_px)) %||% 8
  
  transparent_bg <- input[[paste0(prefix, "_transparent_bg_", key)]]
  if (is.null(transparent_bg)) transparent_bg <- defaults$transparent_bg %||% FALSE
  transparent_bg <- isTRUE(transparent_bg)
  
  fit_key_size <- legend_key_size
  fit_legend_text <- legend_text
  
  # Shrink text a little on very dense facet grids
  n_panels <- tryCatch({
    built <- ggplot2::ggplot_build(p)
    nrow(built$layout$layout)
  }, error = function(e) 1L)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  axis_text  <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))
  
  p <- p + theme(
    axis.text.x  = element_text(size = axis_text, angle = x_rotation, vjust = x_vjust, hjust = x_hjust, face = "bold"),
    axis.text.y  = element_text(size = axis_text, margin = margin(r = 2, unit = "pt")),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title   = element_text(size = plot_title),
    strip.text   = element_text(size = strip_text, face = "bold"),
    legend.text  = element_text(size = fit_legend_text),
    legend.title = element_text(size = legend_title),
    legend.position = if (show_legend) "top" else "none",
    legend.justification = "right",
    legend.box.just = "right",
    legend.key.width  = unit(fit_key_size, "cm"),
    legend.key.height = unit(fit_key_size, "cm"),
    legend.spacing.x  = unit(1, "pt"),
    panel.spacing.x = grid::unit(panel_gap_px, "pt"),
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )
  
  if (transparent_bg) {
    p <- p + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )
  }
  
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomText")) {
      p$layers[[i]]$aes_params$size     <- data_label_size
      p$layers[[i]]$aes_params$colour   <- data_label_colour
      p$layers[[i]]$aes_params$fontface <- "bold"
    }
  }
  
  if (!isTRUE(show_labels)) {
    is_text_layer <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
    p$layers <- p$layers[!is_text_layer]
  }
  
  p <- p + coord_cartesian(clip = "off")
  
  attr(p, "vulsen_bg")        <- if (transparent_bg) "transparent" else "white"
  attr(p, "vulsen_width_in")  <- width_in
  attr(p, "vulsen_height_in") <- height_in
  attr(p, "vulsen_dpi")       <- dpi
  attr(p, "vulsen_panel_gap_px") <- panel_gap_px
  
  p
}

# ---- Preprocessing for heatmap data ----
# If yvar column is missing, return empty data frame.
prep_heatmap_df <- function(df, yvar, class_label, model_1, model_2) {
  if (!(yvar %in% names(df))) {
    return(data.frame())
  }
  d <- as.data.frame(df)
  d <- label_classes(d)
  if (class_label %in% names(ORDER_LIST)) {
    d$Description <- factor(as.character(d$Description), levels = ORDER_LIST[[class_label]])
  }
  # Do not impose region order; keep as-is.
  if (yvar == "Region") {
    # Use the actual unique values in the order they appear (or sort alphabetically)
    d[[yvar]] <- factor(as.character(d[[yvar]]), levels = unique(as.character(d[[yvar]])))
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
add_rel_bin <- function(d, breaks, intervals, colours) {
  cutv <- cut(d$value, breaks = breaks, include.lowest = TRUE)
  lev <- levels(cutv)
  map <- data.frame(Relative_ALL = lev, Mapping = intervals[seq_along(lev)], stringsAsFactors = FALSE)
  d$Relative_ALL <- factor(map$Mapping[match(as.character(cutv), map$Relative_ALL)], levels = intervals)
  d$fill_col <- colours[match(d$Relative_ALL, intervals)]
  yv <- names(d)[names(d) %in% c("Region", "STATECODE")][1]
  d$hover <- paste0(
    "Primary Modifier: ", d$Description,
    "<br>", ifelse(yv == "Region", "Region", "State"),
    ": ", d[[yv]],
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
    colours,
    model_1,
    model_2,
    scope = c("region", "state"),
    side_by_side = FALSE,
    n_models = 2
) {
  scope <- match.arg(scope)
  d <- prep_heatmap_df(df, yvar, class_label, model_1, model_2)
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  d <- add_rel_bin(d, breaks, intervals, colours)
  
  nx <- length(unique(d$Description))
  ny <- length(unique(d[[yvar]]))
  sz <- compute_dynamic_sizes(nx, ny, scope)
  
  ttl <- if (isTRUE(side_by_side)) paste0(title, " - side-by-side focus") else title
  facet_ncol <- if (isTRUE(n_models == 1)) 1 else 2
  
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = Description, y = .data[[yvar]], fill = Relative_ALL, text = hover)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.22) +
    ggplot2::facet_wrap(~variable, ncol = facet_ncol) +
    ggplot2::geom_text(
      ggplot2::aes(label = value_lab),
      color = "white", size = sz$matrix_label, fontface = "bold", na.rm = TRUE
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(n.dodge = 1)) +
    ggplot2::scale_y_discrete(expand = c(0, 0), drop = FALSE) +
    ggplot2::scale_fill_manual(values = stats::setNames(colours, intervals), limits = intervals, drop = FALSE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) +
    ggplot2::labs(title = ttl, fill = "Relative AAL") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      aspect.ratio = 1,
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "#000000",
                                         size = ifelse(scope == "state", 19, 23), margin = ggplot2::margin(t = 0, b = 10)),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = ifelse(scope == "state", 12, 16), margin = ggplot2::margin(t = 2, b = 1)),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.3),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10.2),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = sz$axis_x, angle = 0, hjust = 0.5, face = "bold", margin = ggplot2::margin(t = 1)),
      axis.text.y = ggplot2::element_text(size = sz$axis_y, face = "bold"),
      plot.margin = grid::unit(c(0.02, 0.04, 0.02, 0.01), "in")
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
add_pct_bin <- function(d, breaks, intervals, colours) {
  cutv <- cut(d$value, breaks = breaks, include.lowest = TRUE)
  lev <- levels(cutv)
  map <- data.frame(Relative_ALL = lev, Mapping = intervals[seq_along(lev)], stringsAsFactors = FALSE)
  d$Relative_ALL <- factor(map$Mapping[match(as.character(cutv), map$Relative_ALL)], levels = intervals)
  d$hover <- paste0(
    "Primary Modifier: ", d$Description,
    "<br>Region: ", d$Region,
    "<br>% change: ", sprintf("%.2f", d$value)
  )
  d$value_lab <- ifelse(is.na(d$value), "", sprintf("%.1f", d$value))
  d
}

# ---- Percentage change heatmap (ggplot) ----
plot_pct_gg <- function(d, class_label, breaks, intervals, colours) {
  if (is.null(d) || nrow(d) == 0) {
    return(NULL)
  }
  d$Classification <- class_label
  
  if (class_label %in% names(ORDER_LIST)) {
    d$Description <- factor(as.character(d$Description), levels = ORDER_LIST[[class_label]])
  }
  
  # Use actual region values; no hard-coded order
  d$Region <- factor(as.character(d$Region), levels = unique(as.character(d$Region)))
  
  d <- add_pct_bin(d, breaks, intervals, colours)
  
  nx <- length(unique(d$Description))
  ny <- length(unique(d$Region))
  sz <- compute_dynamic_sizes(nx, ny, "pct")
  
  ggplot2::ggplot(d, ggplot2::aes(x = Description, y = Region, fill = Relative_ALL, text = hover)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.22) +
    ggplot2::geom_text(ggplot2::aes(label = value_lab), color = "white", size = sz$matrix_label, fontface = "bold", na.rm = TRUE) +
    ggplot2::scale_x_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(n.dodge = 1)) +
    ggplot2::scale_y_discrete(expand = c(0, 0), drop = FALSE) +
    ggplot2::scale_fill_manual(values = stats::setNames(colours, intervals), limits = intervals, drop = FALSE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) +
    ggplot2::labs(title = class_label, fill = "% change") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      aspect.ratio = 1,
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "#000000", size = 23, margin = ggplot2::margin(t = 0, b = 10)),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.3),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10.2),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = sz$axis_x, angle = 0, hjust = 0.5, face = "bold", margin = ggplot2::margin(t = 1)),
      axis.text.y = ggplot2::element_text(size = sz$axis_y, face = "bold"),
      plot.margin = grid::unit(c(0.02, 0.04, 0.02, 0.01), "in")
    )
}

# ---- Get plot dimensions for UI ----
plot_dims <- function(data, scope = c("region", "state", "pct")) {
  scope <- match.arg(scope)
  yvar <- if (scope == "state") "STATECODE" else "Region"
  n_y <- length(unique(data[[yvar]]))
  n_x <- length(unique(data$Description))
  
  base_h <- plot_height_for(scope, n_y, n_x)
  if (n_x <= 3 && scope %in% c("region", "pct")) base_h <- "700px"
  if (n_x <= 3 && scope == "state") base_h <- "850px"
  list(height = base_h, n_y = n_y, n_x = n_x)
}

# ---- Build Regionwise + Statewise plots only ----
build_rel_state_gplots <- function(obj, side_by_side = FALSE, rel_scheme = NULL) {
  n_models <- obj$n_models %||% 2
  
  rel_scheme <- rel_scheme %||% REL_AAL_DEFAULT_BINS
  rel_vec <- vul_scheme_to_vectors(rel_scheme)
  
  # Region plots: skip if region_split is empty
  region <- list()
  if (!is.null(obj$region_split) && length(obj$region_split) > 0) {
    region <- lapply(names(obj$region_split), function(nm) {
      df <- obj$region_split[[nm]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      plot_rel_gg(
        df, "Region", nm, nm,
        rel_vec$breaks, rel_vec$intervals, rel_vec$colours,
        obj$model_1, obj$model_2, "region", side_by_side, n_models
      )
    })
    names(region) <- names(obj$region_split)
    # Remove NULL entries
    region <- region[!sapply(region, is.null)]
  }
  
  # State plots (always attempted)
  state <- lapply(names(obj$state_split), function(nm) {
    df <- obj$state_split[[nm]]
    if (is.null(df) || nrow(df) == 0) return(NULL)
    plot_rel_gg(
      df, "STATECODE", nm, nm,
      rel_vec$breaks, rel_vec$intervals, rel_vec$colours,
      obj$model_1, obj$model_2, "state", side_by_side, n_models
    )
  })
  names(state) <- names(obj$state_split)
  state <- state[!sapply(state, is.null)]
  
  list(region = region, state = state)
}

# ---- Build Percentage Change plots only ----
build_pct_gplots <- function(obj, pct_scheme = NULL) {
  n_models <- obj$n_models %||% 2
  pct_scheme <- pct_scheme %||% PCT_CHANGE_DEFAULT_BINS
  pct_vec <- vul_scheme_to_vectors(pct_scheme)
  
  pct <- list()
  pct_data <- list()
  
  if (n_models == 2 && !is.null(obj$region_split) && length(obj$region_split) > 0) {
    # Only proceed if region data is available
    pct_data <- make_pct_data(obj)
    if (length(pct_data) > 0) {
      pct <- lapply(names(pct_data), function(nm) {
        d <- pct_data[[nm]]
        if (is.null(d) || nrow(d) == 0 || all(is.na(d$value))) return(NULL)
        plot_pct_gg(d, nm, pct_vec$breaks, pct_vec$intervals, pct_vec$colours)
      })
      names(pct) <- names(pct_data)
      pct <- pct[!sapply(pct, is.null)]
    }
  }
  
  list(pct = pct, pct_data = pct_data)
}

# ---- Build all static plots ----
build_all_static_gplots <- function(obj, side_by_side = FALSE, rel_scheme = NULL, pct_scheme = NULL) {
  rs <- build_rel_state_gplots(obj, side_by_side, rel_scheme)
  pc <- build_pct_gplots(obj, pct_scheme)
  list(region = rs$region, state = rs$state, pct = pc$pct, pct_data = pc$pct_data)
}