# =============================================================================
# VulsenAPP_data_logic.R - data reading, validation, standardisation
# Extended with helpers for default override settings and the Legend
# Configuration Manager (scheme <-> vector conversion, quantile bin
# generation, JSON import/export).
# =============================================================================

# ---- Read file ----
read_vulsens_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") return(readRDS(path))
  if (ext %in% c("csv", "txt")) return(data.table::fread(path, data.table = FALSE))
  stop("Unsupported file type. Please upload CSV, TXT, or RDS.")
}

# ---- Required columns ----
required_cols_for <- function(model_family) {
  if (tolower(model_family) == "moody") {
    c("LOCNUM", "LOCNAME", "STATECODE", "COUNTY", "CC_OCC_HT_YB_FA", "Classification", "Description")
  } else {
    c("LocationID", "LocationName", "State", "County", "CC_OCC_HT_YT_FA", "Classification", "Description")
  }
}

# ---- Validate uploaded data ----
validate_uploaded_df <- function(df, model_family, model1_col, model2_col = NULL) {
  nms <- trimws(names(df))
  base_req <- required_cols_for(model_family)
  missing_base <- setdiff(base_req, nms)
  errs <- c()
  if (length(missing_base)) {
    errs <- c(errs, paste("Missing base columns:", paste(missing_base, collapse = ", ")))
  }

  model1_col <- trimws(model1_col %||% "")
  if (!nzchar(model1_col)) {
    errs <- c(errs, "Model-1 AAL column name is required.")
  } else if (!(model1_col %in% nms)) {
    errs <- c(errs, paste0("Model-1 AAL column '", model1_col, "' was not found in the uploaded file."))
  }

  has_model2 <- !is.null(model2_col) && nzchar(trimws(model2_col %||% ""))
  if (has_model2) {
    model2_col <- trimws(model2_col)
    if (!(model2_col %in% nms)) {
      errs <- c(errs, paste0("Model-2 AAL column '", model2_col, "' was not found in the uploaded file."))
    }
  }

  list(
    ok = length(errs) == 0,
    errs = errs,
    model1_col = model1_col,
    model2_col = if (has_model2) model2_col else NULL
  )
}

# ---- Standardise data ----
standardize_vulsens <- function(df, model_family, model1_col, model2_col = NULL) {
  names(df) <- trimws(names(df))
  v <- validate_uploaded_df(df, model_family, model1_col, model2_col)
  if (!v$ok) stop(paste(v$errs, collapse = " | "))

  n_models <- if (is.null(v$model2_col)) 1L else 2L

  if (tolower(model_family) == "moody") {
    out <- df |>
      dplyr::transmute(
        LOCNUM = LOCNUM,
        LOCNAME = LOCNAME,
        STATECODE = as.character(STATECODE),
        COUNTY = COUNTY,
        key = CC_OCC_HT_YB_FA,
        model_new = as.numeric(.data[[v$model1_col]]),
        model_old = if (n_models == 2) as.numeric(.data[[v$model2_col]]) else NA_real_,
        Classification = as.character(Classification),
        Description = as.character(Description),
        Region = if ("Region" %in% names(df)) as.character(Region) else NA_character_,
        model_1 = v$model1_col,
        model_2 = if (n_models == 2) v$model2_col else NA_character_,
        model_title = "Moody's"
      )
  } else {
    out <- df |>
      dplyr::transmute(
        LOCNUM = LocationID,
        LOCNAME = LocationName,
        STATECODE = as.character(State),
        COUNTY = County,
        key = CC_OCC_HT_YT_FA,
        model_new = as.numeric(.data[[v$model1_col]]),
        model_old = if (n_models == 2) as.numeric(.data[[v$model2_col]]) else NA_real_,
        Classification = as.character(Classification),
        Description = as.character(Description),
        Region = if ("Region" %in% names(df)) as.character(Region) else NA_character_,
        model_1 = v$model1_col,
        model_2 = if (n_models == 2) v$model2_col else NA_character_,
        model_title = "Verisk"
      )
  }
  out$Description[out$Description %in% "1994"] <- "<1995"
  out$Region <- ifelse(
    is.na(out$Region) | out$Region == "",
    STATE_REGION$Region[match(out$STATECODE, STATE_REGION$STATECODE)],
    out$Region
  )
  out$Region[is.na(out$Region)] <- "Unmapped"
  out$UniqueID <- paste0(out$LOCNAME, "_", out$Classification, "_", out$Description)
  attr(out, "n_models") <- n_models
  out
}

# ---- Modify descriptions ----
modify_df <- function(x) {
  x <- x[!(x$Description %in% "AutoDealer"), ]
  x$Description[x$Description %in% "Agriculture"] <- "AGR"
  x$Description[x$Description %in% "MHwthTie"] <- "MH"
  x$Description[x$Description %in% "AutoPersonal"] <- "Auto"
  x$Description[x$Description %in% "GenCOM"] <- "COM"
  x$Description[x$Description %in% "GenIND"] <- "IND"
  x
}

# ---- Summarise by state or region ----
summarise_average <- function(final_comp, by_region = FALSE, n_models = 2) {
  grp <- if (by_region) {
    c("Region", "Classification", "Description", "key")
  } else {
    c("STATECODE", "Classification", "Description", "key")
  }
  final_comp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      model_new = mean(model_new, na.rm = TRUE),
      model_old = if (n_models == 2) mean(model_old, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(Classification, Description) |>
    modify_df()
}

# ---- Safe ratio calculation ----
safe_ratio <- function(value, desc, baseline) {
  b <- value[desc %in% baseline]
  if (!length(b) || is.na(b[1]) || b[1] == 0) return(rep(NA_real_, length(value)))
  value / b[1]
}

# ---- Build relative data ----
unk_vulfile <- function(avg, byregion, n_models = 2) {
  avg[[byregion]] <- as.character(avg[[byregion]])
  cc <- avg[avg$Classification %in% c("CC", "CC_COM"), ] |>
    dplyr::group_by(Classification, .data[[byregion]]) |>
    dplyr::mutate(
      new_rel = safe_ratio(model_new, Description, "Wood"),
      old_rel = if (n_models == 2) safe_ratio(model_old, Description, "Wood") else NA_real_
    ) |>
    dplyr::ungroup()

  other <- avg[!(avg$Classification %in% c("CC", "CC_COM")), ] |>
    dplyr::group_by(Classification, .data[[byregion]]) |>
    dplyr::mutate(
      new_rel = safe_ratio(model_new, Description, "Unk"),
      old_rel = if (n_models == 2) safe_ratio(model_old, Description, "Unk") else NA_real_
    ) |>
    dplyr::ungroup()

  fc <- dplyr::bind_rows(cc, other)

  if (n_models == 2) {
    unk_comp <- fc |>
      dplyr::select(Description, Classification, dplyr::all_of(byregion), New = new_rel, Old = old_rel) |>
      tidyr::pivot_longer(cols = c("New", "Old"), names_to = "variable", values_to = "value")
  } else {
    unk_comp <- fc |>
      dplyr::select(Description, Classification, dplyr::all_of(byregion), New = new_rel) |>
      tidyr::pivot_longer(cols = c("New"), names_to = "variable", values_to = "value")
  }

  list(raw = fc, unk_comp = unk_comp)
}

# ---- Label classes ----
label_classes <- function(df) {
  df$Classification <- dplyr::recode(
    as.character(df$Classification),
    !!!CLASS_LABELS,
    .default = as.character(df$Classification)
  )
  df
}

# ---- Split by classification ----
split_by_class <- function(unk_comp) {
  tmp <- label_classes(unk_comp)
  cls <- intersect(PLOT_ORDER, unique(tmp$Classification))
  out <- lapply(cls, function(cl) {
    tmp[tmp$Classification == cl, ] |>
      dplyr::arrange(variable, Description)
  })
  names(out) <- cls
  out
}

# =============================================================================
# ---- Legend Configuration Manager -----------------------------------------
#
# A "scheme" is a data.frame with columns: level, label, lower, upper, colour
# (see REL_AAL_DEFAULT_BINS / PCT_CHANGE_DEFAULT_BINS in VulsenAPP_config.R).
# =============================================================================

# ---- Coerce + sort a scheme data.frame into plot-ready vectors ----
vul_scheme_to_vectors <- function(scheme_df) {
  df <- scheme_df
  df$level  <- suppressWarnings(as.numeric(df$level))
  df$lower  <- suppressWarnings(as.numeric(df$lower))
  df$upper  <- suppressWarnings(as.numeric(df$upper))
  df$label  <- as.character(df$label)
  df$colour <- as.character(df$colour)
  df <- df[order(df$lower), ]
  breaks <- c(df$lower[1], df$upper)
  if (!is.infinite(breaks[1])) breaks[1] <- -Inf
  if (!is.infinite(breaks[length(breaks)])) breaks[length(breaks)] <- Inf
  
  list(breaks = breaks, intervals = df$label, colours = df$colour, df = df)
}

# ---- Sanity-check a scheme before it replaces the active reactive value ----
vul_validate_scheme <- function(scheme_df) {
  req_cols <- c("level", "label", "lower", "upper", "colour")
  if (!is.data.frame(scheme_df) || !all(req_cols %in% names(scheme_df))) {
    return(list(ok = FALSE, msg = "Legend scheme is missing required columns.", scheme = NULL))
  }
  if (nrow(scheme_df) < 1) {
    return(list(ok = FALSE, msg = "Legend scheme has no bins.", scheme = NULL))
  }
  df <- scheme_df[req_cols]
  df$level  <- suppressWarnings(as.numeric(df$level))
  df$lower  <- suppressWarnings(as.numeric(df$lower))
  df$upper  <- suppressWarnings(as.numeric(df$upper))
  df$label  <- as.character(df$label)
  df$colour <- as.character(df$colour)
  if (any(is.na(df$lower)) || any(is.na(df$upper))) {
    return(list(ok = FALSE, msg = "Lower/Upper must be numeric (Inf and -Inf are allowed).", scheme = NULL))
  }
  if (any(df$lower >= df$upper)) {
    return(list(ok = FALSE, msg = "Each bin's Lower value must be less than its Upper value.", scheme = NULL))
  }
  if (any(!nzchar(df$colour))) {
    return(list(ok = FALSE, msg = "Every bin needs a colour.", scheme = NULL))
  }
  list(ok = TRUE, msg = "", scheme = df)
}

# ---- Quantile-based automatic bin generation (Problem 3) ----
vul_quantile_bins <- function(values, n_bins, base_colours = NULL) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) {
    stop("No numeric values are available to compute quantile bins from.")
  }
  n_bins <- max(2L, min(20L, round(as.numeric(n_bins))))
  probs <- seq(0, 1, length.out = n_bins + 1)
  qs <- stats::quantile(values, probs = probs, na.rm = TRUE, type = 7, names = FALSE)
  qs <- unique(qs)
  if (length(qs) < 2) {
    stop("Not enough distinct values in the current data to create that many bins.")
  }
  n_bins <- length(qs) - 1
  qs[1] <- -1e12
  qs[length(qs)] <- 1e12

  if (is.null(base_colours)) base_colours <- c("#898D8D", "#003B5E", "#00588D", "#0075BC", "#F0B323", "#C18C0D", "#E07E3C", "#B85B1D", "#7B3D13")
  palette <- grDevices::colorRampPalette(base_colours)(n_bins)

  # Replace only this block inside vul_quantile_bins():
  sentinel_low <- -1e12
  sentinel_high <- 1e12
  
  fmt <- function(x) {
    vapply(x, function(v) {
      if (!is.finite(v)) as.character(v) else format(round(v, 2), nsmall = 0, trim = TRUE)
    }, character(1))
  }
  
  lower <- qs[-length(qs)]
  upper <- qs[-1]
  labels <- ifelse(
    lower == sentinel_low, paste0("<", fmt(upper)),
    ifelse(upper == sentinel_high, paste0(">", fmt(lower)), paste0(fmt(lower), " - ", fmt(upper)))
  )

  data.frame(
    level  = seq_len(n_bins),
    label  = labels,
    lower  = lower,
    upper  = upper,
    colour = palette,
    stringsAsFactors = FALSE
  )
}

# ---- JSON export (Problem 5) ----
vul_scheme_to_json <- function(scheme_df, scheme_name, scheme_type) {
  v <- vul_validate_scheme(scheme_df)
  df <- if (isTRUE(v$ok)) v$scheme else scheme_df
  df <- df[order(-df$level), ]
  bins <- lapply(seq_len(nrow(df)), function(i) {
    list(
      level  = df$level[i],
      label  = df$label[i],
      lower  = if (is.infinite(df$lower[i])) as.character(df$lower[i]) else df$lower[i],
      upper  = if (is.infinite(df$upper[i])) as.character(df$upper[i]) else df$upper[i],
      colour = df$colour[i]
    )
  })
  jsonlite::toJSON(
    list(scheme_name = scheme_name, scheme_type = scheme_type, bins = bins),
    auto_unbox = TRUE, pretty = TRUE
  )
}

# ---- JSON import (Problem 5) ----
vul_scheme_from_json <- function(json_text) {
  parsed <- tryCatch(jsonlite::fromJSON(json_text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$bins) || !length(parsed$bins)) {
    return(list(ok = FALSE, msg = "Could not parse legend JSON (missing 'bins').", scheme_name = NULL, scheme_type = NULL, scheme = NULL))
  }
  rows <- lapply(parsed$bins, function(b) {
    data.frame(
      level  = suppressWarnings(as.numeric(b$level %||% NA)),
      label  = as.character(b$label %||% ""),
      lower  = suppressWarnings(as.numeric(b$lower)),
      upper  = suppressWarnings(as.numeric(b$upper)),
      colour = as.character(b$colour %||% "#898D8D"),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  v <- vul_validate_scheme(df)
  if (!isTRUE(v$ok)) {
    return(list(ok = FALSE, msg = v$msg, scheme_name = parsed$scheme_name %||% NULL, scheme_type = parsed$scheme_type %||% NULL, scheme = NULL))
  }
  list(
    ok = TRUE, msg = "",
    scheme_name = if (!is.null(parsed$scheme_name)) as.character(parsed$scheme_name) else "Imported Scheme",
    scheme_type = if (!is.null(parsed$scheme_type)) as.character(parsed$scheme_type) else NA_character_,
    scheme = v$scheme
  )
}

# ---- Main object builder ----
# NOTE (Problem 1 fix): this used to compute per-classification truncated
# legend breaks via make_custom_breaks() and store them as
# state_breaks/region_breaks, which trimmed the legend down to whatever bins
# were present in each individual plot. The legend is now entirely decoupled
# from data preparation: build_all_static_gplots() (VulsenAPP_plot_functions.R)
# takes the active Relative AAL / Percentage Change scheme directly from the
# Legend Configuration Manager and applies it uniformly to every plot in a
# gallery, so this builder no longer computes or returns any breaks.
build_rmd_objects <- function(raw_df, model_family, model1_col, model2_col = NULL, meta = NULL) {
  final_comp <- standardize_vulsens(raw_df, model_family, model1_col, model2_col)
  n_models <- attr(final_comp, "n_models") %||% (if (is.null(model2_col) || !nzchar(trimws(model2_col %||% ""))) 1L else 2L)

  avg_state <- summarise_average(final_comp, FALSE, n_models)
  avg_region <- summarise_average(final_comp, TRUE, n_models)

  state_obj <- unk_vulfile(avg_state, "STATECODE", n_models)
  region_obj <- unk_vulfile(avg_region, "Region", n_models)

  state_split <- split_by_class(state_obj$unk_comp)
  region_split <- split_by_class(region_obj$unk_comp)

  list(
    final_comp = final_comp,
    avg_state = avg_state,
    avg_region = avg_region,
    state_split = state_split,
    region_split = region_split,
    model_title = unique(final_comp$model_title)[1],
    model_1 = unique(final_comp$model_1)[1],
    model_2 = unique(final_comp$model_2)[1],
    n_models = n_models,
    meta = meta
  )
}

# ---- Default override settings (used in server) ----
# Trimmed to only the controls the app now exposes.
#
# CHANGE LOG (this pass):
#   Problem 3 - `export_w`/`export_h`/`display_w_pct`/`display_h_px`/
#               `card_max_width_px` (three separate, inconsistent sizing
#               paths) are replaced by a single WYSIWYG canvas config:
#               `width_in`, `height_in`, `dpi`, `transparent_bg`,
#               `gap_px`, plus gallery-only `top_margin_px` /
#               `bottom_margin_px` / `left_margin_px` / `right_margin_px`.
#               Screen rendering, gallery layout, and every export path
#               (PNG/SVG/PPT/report) should all read from this same set
#               of values instead of maintaining their own.
#   Problem 2 - `x_rotation` / `x_vjust` added as real controls.
#   Problem 5 - `axis_title` removed. Axis titles are never shown; this is
#               now fixed inside vul_apply_overrides(), not a setting.
#   Problem 6 - `strip_face` removed. Strip text is always bold.
#   Problem 7 - `legend_pos` removed. The legend is always top-positioned
#               when shown (`legend_show` still controls visibility).
#   Problem 8 - `data_label_face` removed. Data labels are always bold.
#
# Controls that only existed to distort the chart (axis line colour, panel
# background, panel/tile border colour & width, panel spacing, background
# swatch, title border) remain removed and fixed at the values the original
# RMarkdown report used, baked directly into the plotting functions instead
# of being threaded through as overrides.
vul_default_overrides <- function(group = c("region", "state", "pct")) {
  group <- match.arg(group)

  by_group <- switch(
    group,
    region = list(axis_text = 12, plot_title = 16, strip_text = 12, legend_text = 10, legend_title = 10),
    state  = list(axis_text = 10, plot_title = 14, strip_text = 10, legend_text = 9,  legend_title = 9),
    pct    = list(axis_text = 12, plot_title = 16, strip_text = 12, legend_text = 10, legend_title = 10)
  )

  c(
    list(
      # ---- Output / Canvas properties (Problem 3 - single source of truth,
      #      used for on-screen rendering AND every export path) ----
      width_in = 9,
      height_in = 5,
      dpi = 300,
      transparent_bg = FALSE,
      gap_px = 16,

      # ---- Plot margins (Gallery Defaults only - no per-plot override
      #      inputs exist for these; individual plots always inherit them) ----
      top_margin_px = 10,
      bottom_margin_px = 10,
      left_margin_px = 10,
      right_margin_px = 10,

      # ---- X-axis label rotation / positioning (Problem 2) ----
      x_rotation = 0,
      x_vjust = 0.5,

      # ---- Legend visibility + sizing (position fixed - Problem 7) ----
      legend_show = TRUE,
      legend_key_size = 0.8,

      # ---- Data label visibility, size, colour (face fixed - Problem 8) ----
      data_label_size = 3.5,
      data_label_colour = "#FFFFFF",
      show_labels = TRUE
    ),
    by_group
  )
}
