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
CLASS_LABELS <- c(
  "CC"="Construction Class", "CC_COM"="Construction Class - COM", "FA_COM"="Floor Area - COM", "FA_SFD"="Floor Area - SFD",
  "HT_COM"="COM-Height", "HT_SFD"="SFD-Height", "OCC"="Occupancy", "YB"="Year Built", "YB_COM"="Year Built - COM"
)
PLOT_ORDER <- c("Construction Class", "Construction Class - COM", "SFD-Height", "COM-Height", "Year Built", "Year Built - COM", "Occupancy", "Floor Area - COM", "Floor Area - SFD")

# ---- HARD-CODED US REGION MAPPING REMOVED ----
# STATE_REGION and REGION_ORDER have been deleted.
# Regions are now either supplied in the main data or added via an optional mapping file.

# ---- ORDER_LIST for factor ordering ----
ORDER_LIST <- list(
  "Construction Class"=c("Auto","MH","LM","Wood","Unk","Masonry","Steel","RC"),
  "Construction Class - COM"=c("Auto","MH","LM","Wood","Unk","Masonry","Steel","RC"),
  "COM-Height"=c("1","3","Unk","5","7","10","15","20"),
  "SFD-Height"=c("1","Unk","2"),
  "Year Built"=c("<1995","Unk","1995","2000","2005","2010","2015","2020","2025"),
  "Year Built - COM"=c("<1995","Unk","1995","2000","2005","2010","2015","2020","2025"),
  "Occupancy"=c("SFD","MFD","COM","Unk","IND"),
  "Floor Area - COM"=c("10000","5000","15000","20000","25000","30000","Unk"),
  "Floor Area - SFD"=c("1000","3000","2000","Unk","5000","7500")
)

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












