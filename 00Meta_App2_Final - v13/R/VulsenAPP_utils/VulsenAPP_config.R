# =============================================================================
# VulsenAPP_config.R
# =============================================================================

# ---- Make peril_lookup() available ----
# This file is sourced from VulsenAPP_v7.R; we load the helper here so that
# PERIL_CHOICES can be defined dynamically and the UI/server can use peril_lookup.
if (file.exists("R/input_preparation_helpers.R")) {
  source("R/input_preparation_helpers.R", local = TRUE)
} else {
  # Fallback definition if the helper is not found (e.g., standalone testing)
  peril_lookup <- function() {
    list(
      Earthquake  = c("AllPeril", "Shake", "Tsunami"),
      Windstorm   = c("AllPeril", "Wind", "StormSurge"),
      SCS         = c("AllPeril", "HA", "TO", "SLW"),
      Winterstorm = c("AllPeril", "Wind", "Ice", "Freeze", "Snow"),
      Flood       = c("AllPeril", "TC", "nonTC"),
      Wildfire    = c("AllPeril", "Fire", "Smoke")
    )
  }
}

APP_TITLE <- "Vulnerability Sensitivity App"
APP_AUTHOR <- "Property Research"

# =============================================================================
# ---- DYNAMIC CLASSIFICATION CONFIG ----------------------------------------
# =============================================================================
# CLASS_LABELS, PLOT_ORDER and ORDER_LIST used to be hardcoded, fixed-length
# vectors/lists. That meant any new Classification code or Description value
# that wasn't already listed here was silently dropped downstream (see
# split_by_class() / intersect(PLOT_ORDER, ...) and the factor(levels=...)
# calls in VulsenAPP_plot_functions.R).
#
# They are now *built from whatever file is uploaded*, alphabetically, so a
# brand-new class or description "just works" without touching this file.
#
# CLASS_LABELS is still a named vector (code -> label) and ORDER_LIST is
# still a named list keyed by label, with the SAME shape as before -- so
# every downstream consumer (VulsenAPP_data_logic.R, VulsenAPP_plot_functions.R,
# Vulsen_server.R) keeps working unchanged. They just read CLASS_LABELS /
# PLOT_ORDER / ORDER_LIST from the global environment at call time, and this
# file now refreshes those globals whenever a new file is processed.
# =============================================================================

# ---- Safe defaults so the app doesn't error before any file is loaded ----
CLASS_LABELS <- character(0)
PLOT_ORDER   <- character(0)
ORDER_LIST   <- list()

#' Natural sort helper.
#'
#' Sorts a character vector alphabetically, but numeric-looking values
#' (e.g. "1","3","10","20" or "1995","2000") are compared as numbers so
#' "10" doesn't sort before "2". A small set of generic, non-business-
#' specific conventions are applied so the ordering still reads sensibly:
#'   - values starting with "<" (e.g. "<1995") sort first
#'   - the literal "Unk" sorts last
#'   - everything else: numeric values sort numerically, text values
#'     sort alphabetically (case-insensitive), numbers before text
#' No class/description names are hardcoded here -- this works for any
#' input file.
natural_sort_values <- function(values) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]

  lt_flag   <- grepl("^<", values)
  unk_flag  <- toupper(values) == "UNK"
  rest_flag <- !lt_flag & !unk_flag

  lt_vals   <- sort(values[lt_flag])
  unk_vals  <- values[unk_flag]

  rest_vals <- values[rest_flag]
  num_ok    <- suppressWarnings(!is.na(as.numeric(rest_vals)))
  numeric_vals <- rest_vals[num_ok][order(as.numeric(rest_vals[num_ok]))]
  text_vals    <- rest_vals[!num_ok][order(toupper(rest_vals[!num_ok]))]

  c(lt_vals, numeric_vals, text_vals, unk_vals)
}

#' Turn a raw Classification code into a display label.
#'
#' No hardcoded lookup table: underscores become " - " and the code is
#' otherwise left as-is (e.g. "CC_COM" -> "CC - COM"). Pass a named
#' `label_overrides` vector (code = "Pretty Label") if you ever want to
#' prettify specific codes without touching this file's logic; anything
#' not present in `label_overrides` falls back to the generic rule.
prettify_class_code <- function(code, label_overrides = NULL) {
  if (!is.null(label_overrides) && code %in% names(label_overrides)) {
    return(unname(label_overrides[[code]]))
  }
  gsub("_", " - ", code, fixed = TRUE)
}

#' Build CLASS_LABELS / PLOT_ORDER / ORDER_LIST from an uploaded data frame.
#'
#' @param df A standardized data frame containing at least `Classification`
#'   and `Description` columns (i.e. the output of standardize_vulsens()).
#' @param label_overrides Optional named character vector, code -> pretty
#'   label, for cases where you want nicer display names than the generic
#'   "underscore to dash" rule. Not required.
#' @return list(labels = CLASS_LABELS, order = PLOT_ORDER, order_list = ORDER_LIST)
build_class_config <- function(df, label_overrides = NULL) {
  if (is.null(df) || !all(c("Classification", "Description") %in% names(df)) || nrow(df) == 0) {
    return(list(labels = character(0), order = character(0), order_list = list()))
  }

  codes <- natural_sort_values(df$Classification)

  labels <- vapply(codes, prettify_class_code, character(1), label_overrides = label_overrides)
  names(labels) <- codes  # CLASS_LABELS: code -> label, alphabetical by code

  plot_order <- unname(labels)  # PLOT_ORDER: labels, alphabetical (by code)

  order_list <- lapply(codes, function(code) {
    natural_sort_values(df$Description[df$Classification == code])
  })
  names(order_list) <- unname(labels)  # ORDER_LIST keyed by label, matching CLASS_LABELS values

  list(labels = labels, order = plot_order, order_list = order_list)
}

#' Apply a built config, refreshing the global CLASS_LABELS / PLOT_ORDER /
#' ORDER_LIST used throughout VulsenAPP_data_logic.R, VulsenAPP_plot_functions.R
#' and Vulsen_server.R.
#'
#' Call this once per uploaded file (e.g. right after standardize_vulsens()
#' in the server), typically:
#'   cfg <- build_class_config(standardized_df)
#'   apply_class_config(cfg)
apply_class_config <- function(cfg) {
  CLASS_LABELS <<- cfg$labels
  PLOT_ORDER   <<- cfg$order
  ORDER_LIST   <<- cfg$order_list
  invisible(cfg)
}

# ---- ORDER_LIST note ----
# VulsenAPP_data_logic.R is left as-is for now: modify_df() and the
# Wood/Unk baseline in unk_vulfile() stay hardcoded by design (out of
# scope for this change).

# =============================================================================
# ---- PER-PLOT X-AXIS CLASS REORDER / RENAME -------------------------------
# =============================================================================
# Each gallery plot card (region/state/pct x class, e.g. "region"+"CC") gets
# its own reorder/rename editor (opened via a button in its collapsible
# override panel, see vul_class_reorder_ui() in VulsenAPP_ui_helpers.R),
# seeded from that plot's ORDER_LIST[[key]] (key == the plot's
# Classification label, same `key`/`class_label` already threaded through
# plot_rel_gg() / plot_pct_gg() / vul_apply_overrides()). Each card's state
# is independent - editing the order/labels for region-CC does not affect
# state-CC or pct-CC.
#
# Shape returned is intentionally the same shape consumed downstream:
#   - vul_class_order_modal_ui() (VulsenAPP_ui_helpers.R) seeds the modal's
#     editable table (Order arrows + locked Name + editable Rename) and its
#     chip preview row from it - reordering/renaming inside the modal is
#     pure client-side JS against this shape, only round-tripping to Shiny
#     once, on Apply
#   - class_order_committed[[ck]] (Vulsen_server.R's
#     setup_class_order_manager()) holds the committed version of it -
#     this is what vul_apply_overrides() (VulsenAPP_plot_functions.R) reads
#     to build scale_x_discrete(limits = name, labels = rename)
# =============================================================================

#' Build the default (identity) class order/rename state for one plot card.
#'
#' @param key The plot's Classification label (i.e. the `class_label` /
#'   `key` already used to index ORDER_LIST elsewhere, e.g. "CC"). Also
#'   accepts NULL/unknown keys gracefully (returns an empty, zero-row
#'   state) so callers don't need to guard before calling this.
#' @return A data.frame with columns:
#'   - order  : integer, 1-based display position (this is what the
#'              Order-column up/down arrows mutate)
#'   - name   : character, the original Description value as it appears in
#'              the data (e.g. "Auto") - never edited by the user, used to
#'              match rows back to the underlying data / scale limits
#'   - rename : character, the user-editable display label - defaults to
#'              `name` until the user types something else
#' No class/description names are hardcoded here, matching the rest of
#' this file's "derive everything from the uploaded file" approach.
default_class_order_state <- function(key) {
  codes <- if (!is.null(key) && length(key) == 1 && key %in% names(ORDER_LIST)) {
    ORDER_LIST[[key]]
  } else {
    character(0)
  }

  if (!length(codes)) {
    return(data.frame(
      order  = integer(0),
      name   = character(0),
      rename = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    order  = seq_along(codes),
    name   = codes,
    rename = codes,
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Legend Configuration Manager - canonical default bin definitions.
# -----------------------------------------------------------------------------
REL_AAL_DEFAULT_BINS <- data.frame(
  level  = 9:1,
  label  = c(">5", "2 - 5", "1.5 - 2", "1.2 - 1.5", "1.05 - 1.2", "0.95 - 1.05", "0.8 - 0.95", "0.5 - 0.8", "0 - 0.5"),
  lower  = c(5, 2, 1.5, 1.2, 1.05, 0.95, 0.8, 0.5, 0),
  upper  = c(Inf, 5, 2, 1.5, 1.2, 1.05, 0.95, 0.8, 0.5),
  colour = c("#7B3D13", "#B85B1D", "#E07E3C", "#C18C0D", "#F0B323", "#0075BC", "#00588D", "#003B5E", "#898D8D"),
  stringsAsFactors = FALSE
)

PCT_CHANGE_DEFAULT_BINS <- data.frame(
  level  = 9:1,
  label  = c(">100", "50 - 100", "25 - 50", "10 - 25", "0 - 10", "-10 - 0", "-25 - -10", "-50 - -25", "< -50"),
  lower  = c(100, 50, 25, 10, 0, -10, -25, -50, -Inf),
  upper  = c(Inf, 100, 50, 25, 10, 0, -10, -25, -50),
  colour = c("#7B3D13", "#E07E3C", "#C18C0D", "#F0B323", "#898D8D", "#4AA57F", "#00588D", "#003B5E", "#0F1B3D"),
  stringsAsFactors = FALSE
)

# Legacy vectors - kept for reference only
REL_BREAKS <- c(0,0.5,0.8,0.95,1.05,1.2,1.5,2,5,Inf)
REL_INTERVALS <- c("0 - 0.5","0.5 - 0.8","0.8 - 0.95","0.95 - 1.05","1.05 - 1.2","1.2 - 1.5","1.5 - 2","2 - 5",">5")
PCT_BREAKS <- c(-Inf,-50,-25,-10,0,10,25,50,100,Inf)
PCT_INTERVALS <- c("< -50","-50 - -25","-25 - -10","-10 - 0","0 - 10","10 - 25","25 - 50","50 - 100",">100")
GAL_COLORS <- c("#898D8D", "#003B5E", "#00588D", "#0075BC", "#F0B323", "#C18C0D", "#E07E3C", "#B85B1D", "#7B3D13")
PCT_COLORS <- c("#0F1B3D", "#003B5E", "#00588D", "#4AA57F", "#898D8D", "#F0B323", "#C18C0D", "#E07E3C", "#7B3D13")

# ---- Model Configuration panel constants ----
VENDOR_CHOICES <- c("Moody's" = "moody", "Verisk" = "verisk")
N_MODEL_CHOICES <- c("1" = "1", "2" = "2")

# ---- Dynamic peril choices (derived from peril_lookup) ----
PERIL_CHOICES <- names(peril_lookup())

DEFAULT_COUNTRY_CODE <- "US"
DEFAULT_SUFFIX <- "2026"

# Default AAL value-column names offered per vendor/model slot.
MODEL1_COL_DEFAULT <- c(verisk = "v13", moody = "HDv1")
MODEL2_COL_DEFAULT <- c(verisk = "v12", moody = "RLv25")



# -----------------------------------------------------------------------------
# Default override values for gallery plots (region, state, pct)
# -----------------------------------------------------------------------------
vul_default_overrides <- function(group) {
  # group: "region", "state", or "pct" (not used in this base version,
  # but kept for potential group-specific overrides)
  list(
    width_in           = 9,
    height_in          = 5,
    dpi                = 300,
    transparent_bg     = FALSE,
    panel_gap_px       = 8,
    axis_text          = 9,
    x_rotation         = 0,
    x_vjust            = 0.5,
    plot_title         = 16,
    strip_text         = 9,
    legend_text        = 8,
    legend_title       = 7,
    legend_key_size    = 0.5,
    legend_show        = TRUE,
    show_labels        = TRUE,
    data_label_size    = 3.5,
    data_label_colour  = "#FFFFFF"
  )
}
