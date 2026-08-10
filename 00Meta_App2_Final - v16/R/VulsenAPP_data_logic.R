# =============================================================================
# VulsenAPP_data_logic.R - data reading, validation, standardisation
# Extended with helpers for default override settings and the Legend
# Configuration Manager (scheme <-> vector conversion, quantile bin
# generation, JSON import/export).
#
# Header validation is case-insensitive and tolerant of extra columns;
# Region is optional and is never derived from a hard-coded state table.
# =============================================================================

# ---- Read file (CSV only) ----
read_vulsens_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    return(data.table::fread(path, data.table = FALSE))
  } else {
    stop("Unsupported file type. Please upload a CSV file.")
  }
}

# ---- Required base columns (with aliases for Moody's) ----
# Returned in lowercase for case-insensitive matching.
required_cols_for <- function(model_family) {
  if (tolower(model_family) == "moody") {
    c("locid", "locname", "statecode", "county", "cc_occ_ht_yb_fa", "classification", "description")
  } else {
    c("locationid", "locationname", "state", "county", "cc_occ_ht_yt_fa", "classification", "description")
  }
}

# ---- Build the full expected header list (base + user-provided model columns) ----
get_expected_headers <- function(model_family, model1_col, model2_col = NULL) {
  base <- required_cols_for(model_family)
  model_cols <- c(tolower(trimws(model1_col)))
  if (!is.null(model2_col) && nzchar(trimws(model2_col))) {
    model_cols <- c(model_cols, tolower(trimws(model2_col)))
  }
  unique(c(base, model_cols))
}

# ---- Case-insensitive, flexible header validation ----
# Accepts extra columns; for Moody's, accepts either locid or locnum.
# Region is never required.
validate_csv_headers <- function(df, model_family, model1_col, model2_col = NULL) {
  actual <- tolower(trimws(names(df)))
  
  alias_groups <- list(
    moody = list(
      LOCID     = c("locid", "locnum"),
      LOCNAME   = c("locname", "locationname"),
      STATECODE = c("statecode", "state"),
      COUNTY    = c("county"),
      KEY       = c("cc_occ_ht_yb_fa"),
      CLASS     = c("classification"),
      DESC      = c("description")
    ),
    verisk = list(
      LOCID     = c("locationid", "locid", "locnum"),
      LOCNAME   = c("locationname", "locname"),
      STATECODE = c("state", "statecode"),
      COUNTY    = c("county"),
      KEY       = c("cc_occ_ht_yt_fa", "cc_occ_ht_yb_fa"),
      CLASS     = c("classification"),
      DESC      = c("description")
    )
  )
  
  groups <- if (tolower(model_family) == "moody") alias_groups$moody else alias_groups$verisk
  
  missing <- c()
  for (g in names(groups)) {
    if (!any(groups[[g]] %in% actual)) {
      missing <- c(missing, paste(groups[[g]], collapse = "/"))
    }
  }
  
  model_cols <- c(tolower(trimws(model1_col)))
  if (!is.null(model2_col) && nzchar(trimws(model2_col))) {
    model_cols <- c(model_cols, tolower(trimws(model2_col)))
  }
  missing_model <- model_cols[!model_cols %in% actual]
  if (length(missing_model) > 0) {
    missing <- c(missing, paste("model column(s):", paste(missing_model, collapse = ", ")))
  }
  
  if (length(missing) == 0) {
    return(list(ok = TRUE, msg = "All required columns are present."))
  } else {
    msg <- paste0(
      "The uploaded CSV is missing the following required column groups:\n",
      paste(missing, collapse = "\n"), "\n\n",
      "Please ensure at least one alias per group exists. Extra columns are allowed."
    )
    return(list(ok = FALSE, msg = msg))
  }
}

# ---- Standardise data with flexible, case-insensitive column matching ----
# Region is preserved as-is if present; it is never derived from a
# hard-coded state table and is left NA when the source data lacks it
# (an optional region-mapping join can fill it in later, in the server).
standardize_vulsens <- function(df, model_family, model1_col, model2_col = NULL) {
  names(df) <- trimws(names(df))
  lower_to_actual <- setNames(names(df), tolower(names(df)))
  
  find_col <- function(aliases, lower_to_actual) {
    for (a in aliases) {
      if (a %in% names(lower_to_actual)) {
        return(lower_to_actual[[a]])
      }
    }
    stop("None of the aliases found: ", paste(aliases, collapse = ", "))
  }
  
  v <- validate_csv_headers(df, model_family, model1_col, model2_col)
  if (!v$ok) stop(v$msg)
  
  n_models <- if (is.null(model2_col) || !nzchar(trimws(model2_col %||% ""))) 1L else 2L
  
  model1_actual <- find_col(c(tolower(trimws(model1_col))), lower_to_actual)
  model2_actual <- NULL
  if (n_models == 2) {
    model2_actual <- find_col(c(tolower(trimws(model2_col))), lower_to_actual)
  }
  
  if (tolower(model_family) == "moody") {
    locid_actual     <- find_col(c("locid", "locnum"), lower_to_actual)
    locname_actual   <- find_col(c("locname", "locationname"), lower_to_actual)
    statecode_actual <- find_col(c("statecode", "state"), lower_to_actual)
    county_actual    <- find_col(c("county"), lower_to_actual)
    key_actual       <- find_col(c("cc_occ_ht_yb_fa"), lower_to_actual)
    class_actual     <- find_col(c("classification"), lower_to_actual)
    desc_actual      <- find_col(c("description"), lower_to_actual)
    region_actual    <- if ("region" %in% names(lower_to_actual)) lower_to_actual[["region"]] else NA_character_
    
    out <- df |>
      dplyr::transmute(
        LOCNUM = .data[[locid_actual]],
        LOCNAME = .data[[locname_actual]],
        STATECODE = as.character(.data[[statecode_actual]]),
        COUNTY = .data[[county_actual]],
        key = .data[[key_actual]],
        model_new = as.numeric(.data[[model1_actual]]),
        model_old = if (n_models == 2) as.numeric(.data[[model2_actual]]) else NA_real_,
        Classification = as.character(.data[[class_actual]]),
        Description = as.character(.data[[desc_actual]]),
        Region = if (!is.na(region_actual)) as.character(.data[[region_actual]]) else NA_character_,
        model_1 = model1_actual,
        model_2 = if (n_models == 2) model2_actual else NA_character_,
        model_title = "Moody's"
      )
  } else {
    locid_actual     <- find_col(c("locationid", "locid", "locnum"), lower_to_actual)
    locname_actual   <- find_col(c("locationname", "locname"), lower_to_actual)
    statecode_actual <- find_col(c("state", "statecode"), lower_to_actual)
    county_actual    <- find_col(c("county"), lower_to_actual)
    key_actual       <- find_col(c("cc_occ_ht_yt_fa", "cc_occ_ht_yb_fa"), lower_to_actual)
    class_actual     <- find_col(c("classification"), lower_to_actual)
    desc_actual      <- find_col(c("description"), lower_to_actual)
    region_actual    <- if ("region" %in% names(lower_to_actual)) lower_to_actual[["region"]] else NA_character_
    
    out <- df |>
      dplyr::transmute(
        LOCNUM = .data[[locid_actual]],
        LOCNAME = .data[[locname_actual]],
        STATECODE = as.character(.data[[statecode_actual]]),
        COUNTY = .data[[county_actual]],
        key = .data[[key_actual]],
        model_new = as.numeric(.data[[model1_actual]]),
        model_old = if (n_models == 2) as.numeric(.data[[model2_actual]]) else NA_real_,
        Classification = as.character(.data[[class_actual]]),
        Description = as.character(.data[[desc_actual]]),
        Region = if (!is.na(region_actual)) as.character(.data[[region_actual]]) else NA_character_,
        model_1 = model1_actual,
        model_2 = if (n_models == 2) model2_actual else NA_character_,
        model_title = "Verisk"
      )
  }
  
  out$Description[out$Description %in% "1994"] <- "<1995"
  
  # DO NOT derive Region from any hard-coded mapping; keep as supplied,
  # normalising blank strings to NA so downstream checks are consistent.
  out$Region <- ifelse(is.na(out$Region) | out$Region == "", NA_character_, out$Region)
  
  out$UniqueID <- paste0(out$LOCNAME, "_", out$Classification, "_", out$Description)
  attr(out, "n_models") <- n_models
  out
}

# ---- Modify descriptions ----
modify_df <- function(x) {
  # x <- x[!(x$Description %in% "AutoDealer"), ]
  # x$Description[x$Description %in% "Agriculture"] <- "AGR"
  # x$Description[x$Description %in% "MHwthTie"] <- "MH"
  # x$Description[x$Description %in% "AutoPersonal"] <- "Auto"
  # x$Description[x$Description %in% "GenCOM"] <- "COM"
  # x$Description[x$Description %in% "GenIND"] <- "IND"
  x
}

# ---- Summarise by state or region ----
# When by_region = TRUE and no usable Region data exists, returns an empty
# data frame (with the expected columns) instead of erroring, so that
# downstream callers can skip region-based output gracefully.
summarise_average <- function(final_comp, by_region = FALSE, n_models = 2) {
  if (by_region) {
    if (!("Region" %in% names(final_comp)) || !any(!is.na(final_comp$Region))) {
      return(final_comp[0, ] |>
               dplyr::select(dplyr::any_of(c("Region", "Classification", "Description", "key"))) |>
               dplyr::mutate(model_new = numeric(0), model_old = numeric(0)))
    }
    final_comp <- final_comp[!is.na(final_comp$Region), ]
    grp <- c("Region", "Classification", "Description", "key")
  } else {
    grp <- c("STATECODE", "Classification", "Description", "key")
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
# If the grouping column (e.g. "Region") is absent or `avg` has no rows,
# returns empty components instead of erroring.
unk_vulfile <- function(avg, byregion, n_models = 2) {
  if (!(byregion %in% names(avg)) || nrow(avg) == 0) {
    return(list(raw = NULL, unk_comp = NULL))
  }
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
# Returns an empty list if there is no data to split (e.g. Region absent).
split_by_class <- function(unk_comp) {
  if (is.null(unk_comp) || nrow(unk_comp) == 0) return(list())
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
# =============================================================================

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
build_rmd_objects <- function(raw_df, model_family, model1_col, model2_col = NULL, meta = NULL) {
  final_comp <- standardize_vulsens(raw_df, model_family, model1_col, model2_col)
  n_models <- attr(final_comp, "n_models") %||% (if (is.null(model2_col) || !nzchar(trimws(model2_col %||% ""))) 1L else 2L)
  
  avg_state <- summarise_average(final_comp, FALSE, n_models)
  avg_region <- summarise_average(final_comp, TRUE, n_models)
  
  state_obj <- unk_vulfile(avg_state, "STATECODE", n_models)
  region_obj <- unk_vulfile(avg_region, "Region", n_models)
  
  state_split <- split_by_class(state_obj$unk_comp)
  region_split <- split_by_class(region_obj$unk_comp)
  
  has_region <- "Region" %in% names(final_comp) && any(!is.na(final_comp$Region))
  
  list(
    final_comp = final_comp,
    avg_state = avg_state,
    avg_region = avg_region,
    state_split = state_split,
    region_split = region_split,
    has_region = has_region,
    model_title = unique(final_comp$model_title)[1],
    model_1 = unique(final_comp$model_1)[1],
    model_2 = unique(final_comp$model_2)[1],
    n_models = n_models,
    meta = meta
  )
}

# ---- Default override settings (used in server) ----
vul_default_overrides <- function(group = c("region", "state", "pct")) {
  group <- match.arg(group)
  
  by_group <- switch(
    group,
    region = list(axis_text = 9, plot_title = 16, strip_text = 9),
    state  = list(axis_text = 9, plot_title = 14, strip_text = 9),
    pct    = list(axis_text = 9, plot_title = 16, strip_text = 9)
  )
  
  c(
    list(
      width_in = 9,
      height_in = 5,
      dpi = 300,
      transparent_bg = FALSE,
      panel_gap_px = 8,
      top_margin_px = 10,
      bottom_margin_px = 10,
      left_margin_px = 10,
      right_margin_px = 10,
      x_rotation = 0,
      x_vjust = 0.5,
      legend_show = TRUE,
      legend_key_size = 0.5,
      data_label_size = 3.5,
      data_label_colour = "#FFFFFF",
      show_labels = TRUE
    ),
    by_group,
    list(legend_text = 8, legend_title = 7)
  )
}