################################################################################
# FILE   3 OF 12
################################################################################
# Name: VulsenAPP_config.R
# Path: R/VulsenAPP_utils/VulsenAPP_config.R
# Type: R
################################################################################

APP_TITLE <- "Vulnerability Sensitivity App"
APP_AUTHOR <- "Property Research"
CLASS_LABELS <- c(
  "CC"="Construction Class", "CC_COM"="Construction Class - COM", "FA_COM"="Floor Area - COM", "FA_SFD"="Floor Area - SFD",
  "HT_COM"="COM-Height", "HT_SFD"="SFD-Height", "OCC"="Occupancy", "YB"="Year Built", "YB_COM"="Year Built - COM"
)
PLOT_ORDER <- c("Construction Class", "Construction Class - COM", "SFD-Height", "COM-Height", "Year Built", "Year Built - COM", "Occupancy", "Floor Area - COM", "Floor Area - SFD")
STATE_REGION <- data.frame(
  STATECODE=c("AZ","CA","ID","NV","OR","UT","WA","CO","KS","NM","OK","TX","AL","AR","GA","LA","MS","MN","MT","NE","ND","SD","WY","CT","ME","MA","NH","NJ","NY","RI","VT","IL","IN","IA","KY","MI","MO","OH","TN","WI","DE","DC","MD","NC","PA","SC","VA","WV","FL"),
  Region=c(rep("West",7),rep("Southern Great Plains",5),rep("Southeast",5),rep("Northern Great Plains",6),rep("Northeast",8),rep("Midwest",9),rep("Mid Atlantic",8),"Florida"),
  stringsAsFactors=FALSE
)
REGION_ORDER <- c("West","Southern Great Plains","Southeast","Northern Great Plains","Northeast","Midwest","Mid Atlantic","Florida","Unmapped")
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
#
# Single source of truth for the default "Relative AAL" and "Percentage
# Change" legends. Both the Gallery Defaults "Load Default" buttons and the
# initial value of each legend's reactive scheme (see Vulsen_server.R) read
# from here.
#
# Columns: level (9=highest,1=lowest), label, lower (inclusive), upper,
# colour (hex fill).
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

# Legacy vectors - kept only for reference / backward compatibility. No
# longer read by the plotting/binning code path, which now goes entirely
# through REL_AAL_DEFAULT_BINS / PCT_CHANGE_DEFAULT_BINS and the reactive
# Legend Configuration Manager schemes built from them.
REL_BREAKS <- c(0,0.5,0.8,0.95,1.05,1.2,1.5,2,5,Inf)
REL_INTERVALS <- c("0 - 0.5","0.5 - 0.8","0.8 - 0.95","0.95 - 1.05","1.05 - 1.2","1.2 - 1.5","1.5 - 2","2 - 5",">5")
PCT_BREAKS <- c(-Inf,-50,-25,-10,0,10,25,50,100,Inf)
PCT_INTERVALS <- c("< -50","-50 - -25","-25 - -10","-10 - 0","0 - 10","10 - 25","25 - 50","50 - 100",">100")
GAL_COLORS <- c("#898D8D", "#003B5E", "#00588D", "#0075BC", "#F0B323", "#C18C0D", "#E07E3C", "#B85B1D", "#7B3D13")
PCT_COLORS <- c("#0F1B3D", "#003B5E", "#00588D", "#4AA57F", "#898D8D", "#F0B323", "#C18C0D", "#E07E3C", "#7B3D13")

# ---- Model Configuration panel constants ----
VENDOR_CHOICES <- c("Moody's" = "moody", "Verisk" = "verisk")
N_MODEL_CHOICES <- c("1" = "1", "2" = "2")
PERIL_CHOICES <- c("Earthquake", "Windstorm", "SCS", "Winterstorm", "Flood", "Wildfire")
SUBPERIL_CHOICES <- c("AllPeril", "HA", "TO", "SLW")
DEFAULT_COUNTRY_CODE <- "us"
DEFAULT_SUFFIX <- "2026"

# Default AAL value-column names offered per vendor/model slot.
MODEL1_COL_DEFAULT <- c(verisk = "v13", moody = "HDv1")
MODEL2_COL_DEFAULT <- c(verisk = "v12", moody = "RLv25")
