# =============================================================================
# VulsenAPP_plot_functions.R - all plotting functions for VulSen heatmaps
# Extended with scheme-driven legend fill and data-label/strip styling
# overrides.
# =============================================================================

# ---- Apply overrides to a ggplot ----
# Trimmed down to the controls the app still exposes (see the comment above
# vul_default_overrides() in VulsenAPP_data_logic.R for the rationale).
vul_apply_overrides <- function(p, input, prefix, key, defaults) {
  axis_text     <- input[[paste0(prefix, "_axis_text_", key)]] %||% defaults$axis_text %||% 12
  axis_title    <- input[[paste0(prefix, "_axis_title_", key)]] %||% defaults$axis_title %||% 14
  plot_title    <- input[[paste0(prefix, "_plot_title_", key)]] %||% defaults$plot_title %||% 16
  strip_text    <- input[[paste0(prefix, "_strip_text_", key)]] %||% defaults$strip_text %||% 12
  strip_face    <- input[[paste0(prefix, "_strip_face_", key)]] %||% defaults$strip_face %||% "bold"
  legend_text   <- input[[paste0(prefix, "_legend_text_", key)]] %||% defaults$legend_text %||% 10
  legend_title  <- input[[paste0(prefix, "_legend_title_", key)]] %||% defaults$legend_title %||% 10
  legend_pos    <- input[[paste0(prefix, "_legend_pos_", key)]] %||% defaults$legend_pos %||% "top"
  show_legend   <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size %||% 0.8
  show_labels <- input[[paste0(prefix, "_show_labels_", key)]]
  if (is.null(show_labels)) show_labels <- defaults$show_labels %||% TRUE

  data_label_size   <- input[[paste0(prefix, "_data_label_size_", key)]] %||% defaults$data_label_size %||% 3.5
  data_label_colour <- input[[paste0(prefix, "_data_label_colour_", key)]] %||% defaults$data_label_colour %||% "white"
  data_label_face   <- input[[paste0(prefix, "_data_label_face_", key)]] %||% defaults$data_label_face %||% "bold"

  # Shrink text a little on very dense facet grids so labels stay legible.
  n_panels <- tryCatch({
    built <- ggplot2::ggplot_build(p)
    nrow(built$layout$layout)
  }, error = function(e) 1L)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  axis_text  <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))

  p <- p + theme(
    axis.text.x  = element_text(size = axis_text),
    axis.text.y  = element_text(size = axis_text),
    axis.title.x = element_text(size = axis_title),
    axis.title.y = element_text(size = axis_title),
    plot.title   = element_text(size = plot_title),
    strip.text   = element_text(size = strip_text, face = strip_face),
    legend.text  = element_text(size = legend_text),
    legend.title = element_text(size = legend_title),
    legend.position = if (show_legend) legend_pos else "none",
    legend.key.size = unit(legend_key_size, "cm")
  )

  # Data label styling - size/colour/fontface of the value printed inside
  # each tile (Problem 8). Left untouched: tile fill colours (driven by the
  # active legend scheme), tile borders, and panel/axis chrome, all of
  # which are fixed to the values the original RMarkdown report used.
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomText")) {
      p$layers[[i]]$aes_params$size     <- data_label_size
      p$layers[[i]]$aes_params$colour   <- data_label_colour
      p$layers[[i]]$aes_params$fontface <- data_label_face
    }
  }

  if (!isTRUE(show_labels)) {
    is_text_layer <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
    p$layers <- p$layers[!is_text_layer]
  }

  p <- p + coord_cartesian(clip = "off")
  attr(p, "vulsen_bg") <- "white"
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
# breaks/intervals/colours all come from the active Relative AAL legend
# scheme (see vul_scheme_to_vectors() in VulsenAPP_data_logic.R). Always the
# FULL scheme - no per-plot trimming (Problem 1).
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
    ggplot2::scale_x_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(n.dodge = ifelse(nx > 7, 2, 1))) +
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
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10.2),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.margin = ggplot2::margin(t = 0, b = 0),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = sz$axis_x, angle = 0, hjust = 0.5, face = "bold", margin = ggplot2::margin(t = 1)),
      axis.text.y = ggplot2::element_text(size = sz$axis_y, face = "bold", margin = ggplot2::margin(t = 5, r = 10, b = 10, l = 5)),
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
# breaks/intervals/colours come from the active Percentage Change legend
# scheme, kept completely independent from the Relative AAL scheme.
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
  d$Classification <- class_label

  if (class_label %in% names(ORDER_LIST)) {
    d$Description <- factor(as.character(d$Description), levels = ORDER_LIST[[class_label]])
  }

  d$Region <- factor(as.character(d$Region), levels = REGION_ORDER[REGION_ORDER %in% unique(as.character(d$Region))])

  d <- add_pct_bin(d, breaks, intervals, colours)

  nx <- length(unique(d$Description))
  ny <- length(unique(d$Region))
  sz <- compute_dynamic_sizes(nx, ny, "pct")

  ggplot2::ggplot(d, ggplot2::aes(x = Description, y = Region, fill = Relative_ALL, text = hover)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.22) +
    ggplot2::geom_text(ggplot2::aes(label = value_lab), color = "white", size = sz$matrix_label, fontface = "bold", na.rm = TRUE) +
    ggplot2::scale_x_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(n.dodge = ifelse(nx > 7, 2, 1))) +
    ggplot2::scale_y_discrete(expand = c(0, 0), drop = FALSE) +
    ggplot2::scale_fill_manual(values = stats::setNames(colours, intervals), limits = intervals, drop = FALSE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) +
    ggplot2::labs(title = class_label, fill = "% change") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", color = "#000000", size = 23, margin = ggplot2::margin(t = 0, b = 10)),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8),
      legend.position = "top",
      legend.direction = "horizontal",
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
# Everything driven by the Relative AAL legend scheme. Split out from the
# old build_all_static_gplots() so the app can rebuild just this half when
# only the Relative AAL scheme changes (see build_pct_gplots() below and
# the static_plots_rel / static_plots_pct reactives in Vulsen_server.R) -
# rel_scheme / pct_scheme default to the canonical constants from
# VulsenAPP_config.R so this still works for any caller that doesn't pass
# a live reactive scheme. This is now the ONLY place that calls
# plot_rel_gg() (Problem 1 fix: every Regionwise and Statewise plot
# receives the exact same full scheme).
build_rel_state_gplots <- function(obj, side_by_side = FALSE, rel_scheme = NULL) {
  n_models <- obj$n_models %||% 2

  rel_scheme <- rel_scheme %||% REL_AAL_DEFAULT_BINS
  rel_vec <- vul_scheme_to_vectors(rel_scheme)

  region <- lapply(names(obj$region_split), function(nm) {
    plot_rel_gg(
      obj$region_split[[nm]], "Region", nm, obj$model_title,
      rel_vec$breaks, rel_vec$intervals, rel_vec$colours,
      obj$model_1, obj$model_2, "region", side_by_side, n_models
    )
  })
  names(region) <- names(obj$region_split)

  state <- lapply(names(obj$state_split), function(nm) {
    plot_rel_gg(
      obj$state_split[[nm]], "STATECODE", nm, obj$model_title,
      rel_vec$breaks, rel_vec$intervals, rel_vec$colours,
      obj$model_1, obj$model_2, "state", side_by_side, n_models
    )
  })
  names(state) <- names(obj$state_split)

  list(region = region, state = state)
}

# ---- Build Percentage Change plots only ----
# Driven entirely by the (independent) Percentage Change legend scheme.
build_pct_gplots <- function(obj, pct_scheme = NULL) {
  n_models <- obj$n_models %||% 2
  pct_scheme <- pct_scheme %||% PCT_CHANGE_DEFAULT_BINS
  pct_vec <- vul_scheme_to_vectors(pct_scheme)

  if (n_models == 2) {
    pct_data <- make_pct_data(obj)
    pct <- lapply(names(pct_data), function(nm) {
      plot_pct_gg(pct_data[[nm]], nm, pct_vec$breaks, pct_vec$intervals, pct_vec$colours)
    })
    names(pct) <- names(pct_data)
  } else {
    pct_data <- list()
    pct <- list()
  }

  list(pct = pct, pct_data = pct_data)
}

# ---- Build all static plots ----
# Convenience wrapper kept for any caller that wants everything at once
# (e.g. the HTML report generator) - internally just combines the two
# builders above. The live app uses build_rel_state_gplots() /
# build_pct_gplots() directly (via separate reactives) so a Relative AAL
# legend change doesn't also force a rebuild of the Percentage Change
# gallery, and vice versa.
build_all_static_gplots <- function(obj, side_by_side = FALSE, rel_scheme = NULL, pct_scheme = NULL) {
  rs <- build_rel_state_gplots(obj, side_by_side, rel_scheme)
  pc <- build_pct_gplots(obj, pct_scheme)
  list(region = rs$region, state = rs$state, pct = pc$pct, pct_data = pc$pct_data)
}
