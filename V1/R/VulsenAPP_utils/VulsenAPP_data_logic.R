# =============================================================================
# VulsenAPP_data_logic.R – data reading, validation, standardisation
# Extended with a helper to create default override settings
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
# model1_col / model2_col are the AAL value-column names the user typed in
# the "Model Configuration" panel. model2_col is NULL/"" when the user
# selected the single-model-version option.
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
# model2_col = NULL (or "") means single-model-version mode: model_old /
# the "Old" series are simply not produced downstream.
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
    # Single-model mode: only the "New" series exists, so there is
    # nothing to build a percentage-change comparison from.
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

# ---- Custom breaks for relative plots ----
make_custom_breaks <- function(plot_df) {
  mx <- suppressWarnings(max(plot_df$value, na.rm = TRUE))
  if (!is.finite(mx)) mx <- 1
  idx <- which(!(mx > REL_BREAKS))[1]
  if (is.na(idx)) idx <- length(REL_BREAKS)
  list(breaks = REL_BREAKS[1:idx], intervals = REL_INTERVALS[1:(idx - 1)])
}

# ---- Main object builder ----
# model2_col = NULL / "" => single-model-version mode (n_models = 1):
# no "Old" series, and no Percentage Change comparison is produced.
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
    state_breaks = lapply(state_split, make_custom_breaks),
    region_breaks = lapply(region_split, make_custom_breaks),
    model_title = unique(final_comp$model_title)[1],
    model_1 = unique(final_comp$model_1)[1],
    model_2 = unique(final_comp$model_2)[1],
    n_models = n_models,
    meta = meta
  )
}

# ---- Default override settings (used in server) ----
# Mirrors the constants the Rmd hardcodes per section:
#   Region  -> Plot_Heatmap_grid() called with the "Region" text-size vectors
#   State   -> Plot_Heatmap_grid() called with the "State" text-size vectors
#   Pct     -> Plot_pct_heatmap(), axistextsize = 18, all else at its own defaults
# Only fields that genuinely differ between sections in the Rmd are branched;
# everything else (legend position, tile border colour/width, angle, etc.) is
# fixed in the Rmd's function body and so is identical across all three here.
vul_default_overrides <- function(group = c("region", "state", "pct")) {
  group <- match.arg(group)
  
  by_group <- switch(
    group,
    region = list(axis_text = 28, plot_title = 30, strip_text = 16, legend_text = 26, legend_title = 18, margin_r = 72),
    state  = list(axis_text = 14, plot_title = 26, strip_text = 16, legend_text = 16, legend_title = 18, margin_r = 72),
    pct    = list(axis_text = 18, plot_title = 36, strip_text = 12, legend_text = 26, legend_title = 26, margin_r = 3)
  )
  
  c(
    list(
      w = 9,
      h = 5,
      dpi = 150,
      bg = "white",
      axis_title = 14,
      axis_angle = 0,           # Rmd never rotates axis labels (xtextangle is always 0)
      legend_pos = "top",
      legend_show = TRUE,
      legend_key_size = 0.8,
      title_hjust = 0.5,
      axis_line_col = "black",
      panel_fill = "white",
      grid_col = "white",       # tile border colour - Rmd's geom_tile(color = "white")
      tile_border_lwd = 0.1,    # Rmd's geom_tile(size = 0.1)
      panel_spacing = 0.5,
      margin_t = 0,
      margin_b = 0,
      margin_l = 0,
      border_col = "black",
      border_lwd = 1,           # Rmd's panel.border size = 1
      col_sfd = "#6FACDE",
      col_com = "#F0B323",
      col_pen = "#F0B323",
      col_cred = "#6FACDE",
      axis_text_margin_t = 5,
      axis_text_vjust = 0.5
    ),
    by_group
  )
}



