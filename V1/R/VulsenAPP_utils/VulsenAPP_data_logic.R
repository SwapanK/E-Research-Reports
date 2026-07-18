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

# ---- Value column candidates ----
value_cols_for <- function(model_family, names_vec) {
  if (tolower(model_family) == "moody") {
    new_candidates <- c("HDv1", "HDv1AAL_AllPeril", "HDv1AAL_Hail", "HDv1_AAL_AllPeril", "HDv1_AllPeril", "HDv1AAL")
    old_candidates <- c("RLv25", "DLMv25AAL_AllPeril", "DLMv25AAL_Hail", "RLv25AAL_AllPeril", "DLMv25_AllPeril", "RLv25AAL")
  } else {
    new_candidates <- c("v13", "AIRV13", "TSv13", "Veriskv13", "V13", "AIR_v13")
    old_candidates <- c("v12", "AIRV12", "TSv12", "Veriskv12", "V12", "AIR_v12")
  }
  list(new = intersect(new_candidates, names_vec)[1], old = intersect(old_candidates, names_vec)[1])
}

# ---- Validate uploaded data ----
validate_uploaded_df <- function(df, model_family) {
  nms <- trimws(names(df))
  base_req <- required_cols_for(model_family)
  missing_base <- setdiff(base_req, nms)
  vals <- value_cols_for(model_family, nms)
  errs <- c()
  if (length(missing_base)) {
    errs <- c(errs, paste("Missing base columns:", paste(missing_base, collapse = ", ")))
  }
  if (is.na(vals$new) || is.null(vals$new)) {
    errs <- c(errs, paste(
      "Missing new-version value column. Accepted names include:",
      if (tolower(model_family) == "moody") "HDv1 / HDv1AAL_AllPeril / HDv1AAL_Hail" else "v13 / AIRV13 / TSv13"
    ))
  }
  if (is.na(vals$old) || is.null(vals$old)) {
    errs <- c(errs, paste(
      "Missing comparison-version value column. Accepted names include:",
      if (tolower(model_family) == "moody") "RLv25 / DLMv25AAL_AllPeril / DLMv25AAL_Hail" else "v12 / AIRV12 / TSv12"
    ))
  }
  list(
    ok = length(errs) == 0,
    errs = errs,
    new_col = vals$new,
    old_col = vals$old
  )
}

# ---- Standardise data ----
standardize_vulsens <- function(df, model_family) {
  names(df) <- trimws(names(df))
  v <- validate_uploaded_df(df, model_family)
  if (!v$ok) stop(paste(v$errs, collapse = " | "))
  
  if (tolower(model_family) == "moody") {
    out <- df |>
      dplyr::transmute(
        LOCNUM = LOCNUM,
        LOCNAME = LOCNAME,
        STATECODE = as.character(STATECODE),
        COUNTY = COUNTY,
        key = CC_OCC_HT_YB_FA,
        model_new = as.numeric(.data[[v$new_col]]),
        model_old = as.numeric(.data[[v$old_col]]),
        Classification = as.character(Classification),
        Description = as.character(Description),
        Region = if ("Region" %in% names(df)) as.character(Region) else NA_character_,
        model_1 = "HDv1",
        model_2 = "RLv25",
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
        model_new = as.numeric(.data[[v$new_col]]),
        model_old = as.numeric(.data[[v$old_col]]),
        Classification = as.character(Classification),
        Description = as.character(Description),
        Region = if ("Region" %in% names(df)) as.character(Region) else NA_character_,
        model_1 = "v13",
        model_2 = "v12",
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
summarise_average <- function(final_comp, by_region = FALSE) {
  grp <- if (by_region) {
    c("Region", "Classification", "Description", "key")
  } else {
    c("STATECODE", "Classification", "Description", "key")
  }
  final_comp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      model_new = mean(model_new, na.rm = TRUE),
      model_old = mean(model_old, na.rm = TRUE),
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
unk_vulfile <- function(avg, byregion) {
  avg[[byregion]] <- as.character(avg[[byregion]])
  cc <- avg[avg$Classification %in% c("CC", "CC_COM"), ] |>
    dplyr::group_by(Classification, .data[[byregion]]) |>
    dplyr::mutate(
      new_rel = safe_ratio(model_new, Description, "Wood"),
      old_rel = safe_ratio(model_old, Description, "Wood")
    ) |>
    dplyr::ungroup()
  
  other <- avg[!(avg$Classification %in% c("CC", "CC_COM")), ] |>
    dplyr::group_by(Classification, .data[[byregion]]) |>
    dplyr::mutate(
      new_rel = safe_ratio(model_new, Description, "Unk"),
      old_rel = safe_ratio(model_old, Description, "Unk")
    ) |>
    dplyr::ungroup()
  
  fc <- dplyr::bind_rows(cc, other)
  unk_comp <- fc |>
    dplyr::select(Description, Classification, dplyr::all_of(byregion), New = new_rel, Old = old_rel) |>
    tidyr::pivot_longer(cols = c("New", "Old"), names_to = "variable", values_to = "value")
  
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
build_rmd_objects <- function(raw_df, model_family) {
  final_comp <- standardize_vulsens(raw_df, model_family)
  avg_state <- summarise_average(final_comp, FALSE)
  avg_region <- summarise_average(final_comp, TRUE)
  
  state_obj <- unk_vulfile(avg_state, "STATECODE")
  region_obj <- unk_vulfile(avg_region, "Region")
  
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
    model_2 = unique(final_comp$model_2)[1]
  )
}

# ---- Default override settings (used in server) ----
vul_default_overrides <- function() {
  list(
    w = 9,
    h = 5,
    dpi = 150,
    bg = "white",
    axis_text = 12,
    axis_title = 14,
    plot_title = 16,
    strip_text = 12,
    legend_text = 10,
    legend_title = 10,
    axis_angle = 90,
    legend_pos = "top",
    legend_show = TRUE,
    legend_key_size = 0.8,
    title_hjust = 0.5,
    axis_line_col = "black",
    panel_fill = "white",
    grid_col = "#e9ecf3",
    panel_spacing = 3,
    margin_t = 30,
    margin_r = 10,
    margin_b = 30,
    margin_l = 10,
    border_col = "black",
    border_lwd = 0.5,
    col_sfd = "#6FACDE",
    col_com = "#F0B323",
    col_pen = "#F0B323",
    col_cred = "#6FACDE",
    axis_text_margin_t = 5,
    axis_text_vjust = 0.5
  )
}


