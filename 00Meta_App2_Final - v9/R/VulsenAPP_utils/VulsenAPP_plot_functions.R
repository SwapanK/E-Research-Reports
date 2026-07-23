
# =============================================================================
# VulsenAPP_plot_functions.R - all plotting functions for VulSen heatmaps
# Extended with scheme-driven legend fill and data-label/strip styling
# overrides.
#
# CHANGE LOG (this pass):
#   Problem 1 - X-axis labels no longer auto zig-zag (guide_axis n.dodge
#               forced to 1 in both plot_rel_gg() and plot_pct_gg()).
#   Problem 2 - X-Axis Label Rotation / VJust are now real, user-controlled
#               theme overrides applied in vul_apply_overrides().
#   Problem 3 - vul_apply_overrides() now also accepts width/height/dpi/
#               transparent-background/panel-gap/margin settings and stamps
#               them onto the returned plot object as attributes
#               (vulsen_width_in / vulsen_height_in / vulsen_dpi /
#               vulsen_panel_gap_px / vulsen_bg) so the server layer has a
#               single place to read canvas + export settings from, instead
#               of re-deriving them separately for screen vs. download.
#   Model Panel Gap - the old "Card-to-Card Gap (px)" control (which only
#               ever drove an outer HTML/CSS gap between different gallery
#               cards, via --vul-card-gap) has been removed entirely. It has
#               been replaced by a real "Model Panel Gap (px)" control that
#               is applied directly to the ggplot object as
#               panel.spacing.x, which is the actual horizontal gap between
#               the two model facets (e.g. HDv1 vs RLv25) inside a single
#               heatmap. This is now wired all the way through screen
#               render and PNG export via vul_apply_overrides().
#   Problem 5 - axis.title.x/y are now hard-blanked in the override layer
#               (previously the override function was re-enabling axis
#               titles that the base plot theme had already blanked out -
#               this was a bug, not just a missing feature).
#   Problem 6 - Strip Font Face is no longer a control; always "bold".
#   Problem 7 - Legend Position is no longer a control; always "top" when
#               the legend is shown at all.
#   Problem 8 - Data Label Font Face is no longer a control; always "bold".
# =============================================================================

# ---- Apply overrides to a ggplot ----
# Trimmed down to the controls the app still exposes (see the comment above
# vul_default_overrides() in VulsenAPP_data_logic.R for the rationale).
vul_apply_overrides <- function(p, input, prefix, key, defaults) {
  
  # ---- Text sizing (still user-controlled) ----
  axis_text    <- input[[paste0(prefix, "_axis_text_", key)]]    %||% defaults$axis_text    %||% 12
  plot_title   <- input[[paste0(prefix, "_plot_title_", key)]]   %||% defaults$plot_title   %||% 16
  strip_text   <- input[[paste0(prefix, "_strip_text_", key)]]   %||% defaults$strip_text   %||% 12
  legend_text  <- input[[paste0(prefix, "_legend_text_", key)]]  %||% defaults$legend_text  %||% 10
  legend_title <- input[[paste0(prefix, "_legend_title_", key)]] %||% defaults$legend_title %||% 10
  
  # ---- X-axis label rotation / vjust (Problem 2) ----
  x_rotation <- input[[paste0(prefix, "_x_rotation_", key)]] %||% defaults$x_rotation %||% 0
  x_vjust    <- input[[paste0(prefix, "_x_vjust_", key)]]    %||% defaults$x_vjust    %||% 0.5
  x_rotation <- suppressWarnings(as.numeric(x_rotation)) %||% 0
  x_vjust    <- suppressWarnings(as.numeric(x_vjust))    %||% 0.5
  # Horizontal alignment is intentionally NOT a user control (Problem 2 spec)
  # - once labels are rotated, right-aligning them keeps them tucked under
  # their tick mark instead of drifting; unrotated labels stay centered.
  x_hjust <- if (x_rotation != 0) 1 else 0.5
  
  # ---- Legend visibility / key size (position itself is fixed - Problem 7) ----
  show_legend <- input[[paste0(prefix, "_legend_show_", key)]]
  if (is.null(show_legend)) show_legend <- defaults$legend_show %||% TRUE
  legend_key_size <- input[[paste0(prefix, "_legend_key_size_", key)]] %||% defaults$legend_key_size %||% 0.8
  
  # ---- Value label visibility / size / colour (face is fixed - Problem 8) ----
  show_labels <- input[[paste0(prefix, "_show_labels_", key)]]
  if (is.null(show_labels)) show_labels <- defaults$show_labels %||% TRUE
  data_label_size   <- input[[paste0(prefix, "_data_label_size_", key)]]   %||% defaults$data_label_size   %||% 3.5
  data_label_colour <- input[[paste0(prefix, "_data_label_colour_", key)]] %||% defaults$data_label_colour %||% "#FFFFFF"
  
  # ---- Canvas / export properties (Problem 3 - single source of truth) ----
  width_in  <- input[[paste0(prefix, "_width_in_", key)]]  %||% defaults$width_in  %||% 9
  height_in <- input[[paste0(prefix, "_height_in_", key)]] %||% defaults$height_in %||% 5
  dpi       <- input[[paste0(prefix, "_dpi_", key)]]       %||% defaults$dpi       %||% 300
  # Model Panel Gap (px) - horizontal space between model facets (e.g.
  # HDv1 vs RLv25) inside a single heatmap. Applied below as
  # panel.spacing.x so it actually pushes the facets apart, unlike the old
  # gap_px which only ever drove an unrelated outer-card CSS gap.
  panel_gap_px <- input[[paste0(prefix, "_panel_gap_px_", key)]] %||% defaults$panel_gap_px %||% 16
  panel_gap_px <- suppressWarnings(as.numeric(panel_gap_px)) %||% 16
  
  transparent_bg <- input[[paste0(prefix, "_transparent_bg_", key)]]
  if (is.null(transparent_bg)) transparent_bg <- defaults$transparent_bg %||% FALSE
  transparent_bg <- isTRUE(transparent_bg)
  
  # Margins are a Gallery-Defaults-only control (no per-plot override inputs
  # exist for these), so they always come from `defaults`. Left NULL if the
  # caller's defaults object doesn't define them, in which case the base
  # plot.margin baked into plot_rel_gg()/plot_pct_gg() is left untouched.
  top_margin_px    <- defaults$top_margin_px
  bottom_margin_px <- defaults$bottom_margin_px
  left_margin_px   <- defaults$left_margin_px
  right_margin_px  <- defaults$right_margin_px
  has_margins <- !any(vapply(
    list(top_margin_px, bottom_margin_px, left_margin_px, right_margin_px),
    is.null, logical(1)
  ))
  
  # Shrink text a little on very dense facet grids so labels stay legible.
  n_panels <- tryCatch({
    built <- ggplot2::ggplot_build(p)
    nrow(built$layout$layout)
  }, error = function(e) 1L)
  dense_scale <- if (n_panels > 12) max(0.18, sqrt(12 / n_panels)) else 1
  axis_text  <- max(6, axis_text * max(dense_scale, 0.65))
  strip_text <- max(6, strip_text * max(dense_scale, 0.65))
  
  p <- p + theme(
    axis.text.x  = element_text(size = axis_text, angle = x_rotation, vjust = x_vjust, hjust = x_hjust, face = "bold"),
    axis.text.y  = element_text(size = axis_text),
    # Problem 5: axis titles are never shown, regardless of any legacy
    # setting - do NOT re-introduce an axis_title-driven element_text() here.
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title   = element_text(size = plot_title),
    # Problem 6: strip font face is fixed to bold, not user-configurable.
    strip.text   = element_text(size = strip_text, face = "bold"),
    legend.text  = element_text(size = legend_text),
    legend.title = element_text(size = legend_title),
    # Problem 7: legend position is fixed to "top" whenever shown.
    legend.position = if (show_legend) "top" else "none",
    legend.key.size = unit(legend_key_size, "cm"),
    # Model Panel Gap (px): the actual horizontal space between the two
    # model facets (e.g. HDv1 vs RLv25) inside a single heatmap. Only
    # panel.spacing.x is touched (not plain panel.spacing) so this never
    # affects vertical spacing on plots with stacked facet rows.
    panel.spacing.x = grid::unit(panel_gap_px, "pt")
  )
  
  if (has_margins) {
    p <- p + theme(
      plot.margin = ggplot2::margin(
        t = top_margin_px, r = right_margin_px,
        b = bottom_margin_px, l = left_margin_px, unit = "pt"
      )
    )
  }
  
  if (transparent_bg) {
    p <- p + theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )
  }
  
  # Data label styling - size/colour of the value printed inside each tile
  # (Problem 8: font face is fixed to bold, no longer a control). Left
  # untouched: tile fill colours (driven by the active legend scheme), tile
  # borders, and panel/axis chrome, all of which are fixed to the values the
  # original RMarkdown report used.
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
  
  # Problem 3: stamp the resolved canvas/export config onto the plot object
  # itself so screen rendering and file export both read from the exact
  # same values instead of maintaining two separate sizing paths.
  attr(p, "vulsen_bg")        <- if (transparent_bg) "transparent" else "white"
  attr(p, "vulsen_width_in")  <- width_in
  attr(p, "vulsen_height_in") <- height_in
  attr(p, "vulsen_dpi")       <- dpi
  attr(p, "vulsen_panel_gap_px") <- panel_gap_px
  
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
    # Problem 1: labels always render on a single row - no more automatic
    # zig-zag dodging once nx > 7.
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
      plot.margin = grid::unit(c(0.02, 0.04, 0.02, 0.01), "in")
      # NOTE: panel.spacing.x (the gap between the model facets, e.g. HDv1
      # vs RLv25) is intentionally NOT set here. It is a user-controlled
      # value ("Model Panel Gap (px)") applied later by
      # vul_apply_overrides(), which every code path (screen render + PNG
      # export) already runs on this plot before it's shown/saved.
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
    # Problem 1: labels always render on a single row.
    ggplot2::scale_x_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(n.dodge = 1)) +
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

