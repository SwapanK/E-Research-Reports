# Helper functions for Input File Creation App

library(data.table)
library(dplyr)

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

module_short <- function(module_name) {
  if (identical(module_name, "Vulnerability Sensitivity")) return("Vulsens")
  "Secmod"
}

vendor_meta <- function(vendor, peril) {
  if (vendor == "Moodys") {
    peril_code <- data.frame(
      Peril = c("Earthquake", "Windstorm", "SCS", "Winterstorm", "Flood", "Fire/Wildfire"),
      Code = c("EQ", "WS", "TO", "TO", "FL", "FR"),
      PolicyType = c(1, 2, 3, 3, 4, 5),
      Abb = c("EQ", "TC", "SCS", "WT", "FL", "WF"),
      stringsAsFactors = FALSE
    )
  } else {
    peril_code <- data.frame(
      Peril = c("Earthquake", "Windstorm", "SCS", "Winterstorm", "Flood", "Wildfire"),
      Code = rep("PAL", 6),
      Abb = c("EQ", "TC", "SCS", "WT", "FL", "WF"),
      stringsAsFactors = FALSE
    )
  }
  idx <- which(peril_code$Peril == peril)
  if (!length(idx)) stop("Unsupported peril for selected vendor.")
  list(
    code = peril_code$Code[idx][1],
    abb = peril_code$Abb[idx][1],
    policy_type = if ("PolicyType" %in% names(peril_code)) peril_code$PolicyType[idx][1] else NA
  )
}

sample_paths_input <- function(vendor, module) {
  base <- "data/input_preparation_sample/"
  loc <- paste0(base, "vulsen_loc_USA_USSCS.csv")
  if (vendor == "Moodys" && module == "Vulnerability Sensitivity") {
    comb <- paste0(base, "vulnerability_sensitivity_rms.csv")
  } else if (vendor == "Moodys" && module == "Secondary Modifiers") {
    comb <- paste0(base, "secmod_RMS_SCS_AllPeril.csv")
  } else if (vendor == "Verisk" && module == "Vulnerability Sensitivity") {
    comb <- paste0(base, "vulnerability_sensitivity_air.csv")
  } else {
    comb <- paste0(base, "secmod_AIR_all_peril.csv")
  }
  list(location = loc, combination = comb)
}


read_uploaded_or_sample <- function(uploaded, sample_path) {
  if (!is.null(uploaded) && nzchar(uploaded$datapath)) {
    return(fread(uploaded$datapath))
  }
  fread(sample_path)
}

validate_required_cols <- function(df, required, label) {
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(sprintf("%s file is missing required columns: %s", label, paste(missing, collapse = ", ")))
  }
}

build_output_names <- function(vendor, version, module, country, peril, subperil) {
  short <- module_short(module)
  if (vendor == "Verisk") {
    locfilename <- paste0(country, peril, "_", subperil, "_", short, "_", vendor, "_", version, "_loc")
    accfilename <- paste0(country, peril, "_", subperil, "_", short, "_", vendor, "_", version, "_acc")
  } else {
    locfilename <- paste0(country, peril, "_", short, "_", vendor, "_", version, "_loc")
    accfilename <- paste0(country, peril, "_", short, "_", vendor, "_", version, "_acc")
  }
  list(locfilename = locfilename, accfilename = accfilename)
}

expand_secmods <- function(combination, repeat_n) {
  secmod_start <- match("Description", names(combination)) + 1
  if (is.na(secmod_start) || secmod_start > ncol(combination)) return(data.frame())
  secmod_cols <- names(combination)[secmod_start:ncol(combination)]
  setNames(lapply(secmod_cols, function(col) rep(combination[[col]], repeat_n)), secmod_cols)
}

build_moodys_vulnerability <- function(latlon, combination, args) {
  validate_required_cols(latlon, c("LOCNAME", "Longitude", "Latitude", "StateCode", "County"), "Location")
  validate_required_cols(combination, c("Classification", "USERID1", "USERID2", "BLDGCLASS", "OCCTYPE", "NUMSTORIES", "YEARBUILT"), "Combination")
  
  meta <- vendor_meta("Moodys", args$peril)
  short <- module_short(args$module)
  accountsuffix <- paste0(short, "_", args$subperil, meta$abb, args$suffix)
  fn <- build_output_names("Moodys", args$version, args$module, args$country, args$peril, args$subperil)
  
  n <- nrow(latlon) * nrow(combination)
  cv_cols <- data.frame(
    rep(args$bldgVal, n),
    rep(args$cntVal, n),
    rep(args$BIVal, n)
  )
  names(cv_cols) <- paste0(meta$code, c("CV1VAL", "CV2VAL", "CV3VAL"))
  cv_cols <- cv_cols[, colSums(cv_cols != 0) > 0, drop = FALSE]
  
  loc_comb <- data.frame(
    LOCNUM      = seq_len(n),
    LOCNAME     = rep(latlon$LOCNAME, each = nrow(combination)),
    ACCNTNUM    = rep(paste0(accountsuffix, "_", combination$Classification), nrow(latlon)),
    LONGITUDE   = rep(latlon$Longitude, each = nrow(combination)),
    LATITUDE    = rep(latlon$Latitude, each = nrow(combination)),
    CNTRYCODE   = args$country,
    CNTRYSCHEME = "ISO2A",
    STATECODE   = rep(latlon$StateCode, each = nrow(combination)),
    COUNTY      = rep(latlon$County, each = nrow(combination)),
    OCCSCHEME   = if ("OCCSCHEME" %in% names(combination)) rep(combination$OCCSCHEME, nrow(latlon)) else "ATC",
    OCCTYPE     = rep(combination$OCCTYPE, nrow(latlon)),
    BLDGSCHEME  = if ("BLDGSCHEME" %in% names(combination)) rep(combination$BLDGSCHEME, nrow(latlon)) else "RMS",
    BLDGCLASS   = rep(combination$BLDGCLASS, nrow(latlon)),
    NUMSTORIES  = rep(combination$NUMSTORIES, nrow(latlon)),
    YEARBUILT   = rep(combination$YEARBUILT, nrow(latlon)),
    cv_cols,
    UserID1     = rep(combination[["USERID1"]], nrow(latlon)),
    UserID2     = rep(combination[["USERID2"]], nrow(latlon)),
    check.names = FALSE
  )
  
  accntnum <- unique(paste0(accountsuffix, "_", combination$Classification))
  acc_comb <- data.frame(
    ACCNTNUM   = accntnum,
    ACCNTNAME  = accntnum,
    POLICYNUM  = accntnum,
    POLICYTYPE = rep(meta$policy_type, length(accntnum)),
    check.names = FALSE
  )
  
  list(loc_comb = loc_comb, acc_comb = acc_comb, locfilename = fn$locfilename, accfilename = fn$accfilename)
}

build_moodys_secmod <- function(latlon, combination, args) {
  validate_required_cols(latlon, c("LOCNAME", "Longitude", "Latitude", "StateCode", "County"), "Location")
  validate_required_cols(combination, c("Classification", "Description", "BLDGCLASS", "OCCTYPE", "NUMSTORIES", "YEARBUILT"), "Combination")
  
  meta <- vendor_meta("Moodys", args$peril)
  short <- module_short(args$module)
  accountsuffix <- paste0(short, "_", args$subperil, meta$abb, args$suffix)
  fn <- build_output_names("Moodys", args$version, args$module, args$country, args$peril, args$subperil)
  
  rep_n <- nrow(latlon)
  n <- nrow(latlon) * nrow(combination)
  secmod_df <- expand_secmods(combination, rep_n)
  
  cv_cols <- data.frame(
    rep(args$bldgVal, n),
    rep(args$cntVal, n),
    rep(args$BIVal, n)
  )
  names(cv_cols) <- paste0(meta$code, c("CV1VAL", "CV2VAL", "CV3VAL"))
  cv_cols <- cv_cols[, colSums(cv_cols != 0) > 0, drop = FALSE]
  
  loc_comb <- data.frame(
    LOCNUM      = seq_len(n),
    LOCNAME     = rep(latlon$LOCNAME, each = nrow(combination)),
    ACCNTNUM    = rep(paste0(accountsuffix, "_", combination$Classification), nrow(latlon)),
    LONGITUDE   = rep(latlon$Longitude, each = nrow(combination)),
    LATITUDE    = rep(latlon$Latitude, each = nrow(combination)),
    CNTRYCODE   = args$country,
    CNTRYSCHEME = "ISO2A",
    STATECODE   = rep(latlon$StateCode, each = nrow(combination)),
    COUNTY      = rep(latlon$County, each = nrow(combination)),
    OCCSCHEME   = if ("OCCSCHEME" %in% names(combination)) rep(combination$OCCSCHEME, nrow(latlon)) else "ATC",
    OCCTYPE     = rep(combination$OCCTYPE, nrow(latlon)),
    BLDGSCHEME  = if ("BLDGSCHEME" %in% names(combination)) rep(combination$BLDGSCHEME, nrow(latlon)) else "RMS",
    BLDGCLASS   = rep(combination$BLDGCLASS, nrow(latlon)),
    NUMSTORIES  = rep(combination$NUMSTORIES, nrow(latlon)),
    YEARBUILT   = rep(combination$YEARBUILT, nrow(latlon)),
    cv_cols,
    secmod_df,
    UserID1 = rep(combination[["Description"]], nrow(latlon)),
    check.names = FALSE
  )
  
  accntnum <- unique(paste0(accountsuffix, "_", combination$Classification))
  acc_comb <- data.frame(
    ACCNTNUM   = accntnum,
    ACCNTNAME  = accntnum,
    POLICYNUM  = accntnum,
    POLICYTYPE = rep(meta$policy_type, length(accntnum)),
    check.names = FALSE
  )
  
  list(loc_comb = loc_comb, acc_comb = acc_comb, locfilename = fn$locfilename, accfilename = fn$accfilename)
}

build_verisk_vulnerability <- function(latlon, combination, args) {
  validate_required_cols(latlon, c("LOCNAME"), "Location")
  validate_required_cols(combination, c("Classification", "USERID1", "USERID2"), "Combination")
  
  meta <- vendor_meta("Verisk", args$peril)
  short <- module_short(args$module)
  accountsuffix <- paste0(short, "_", args$subperil, meta$abb, args$suffix)
  fn <- build_output_names("Verisk", args$version, args$module, args$country, args$peril, args$subperil)
  
  n <- nrow(latlon) * nrow(combination)
  cv_cols <- data.frame(
    BuildingValue    = rep(args$bldgVal, n),
    ContentsValue    = rep(args$cntVal, n),
    TimeElementValue = rep(args$BIVal, n),
    check.names = FALSE
  )
  cv_cols <- cv_cols[, colSums(cv_cols != 0) > 0, drop = FALSE]
  
  loc_comb <- data.frame(
    LocationID   = seq_len(n),
    LocationName = rep(latlon$LOCNAME, each = nrow(combination)),
    PolicyID     = rep(paste0(accountsuffix, "_", combination$Classification), nrow(latlon)),
    CountryCode  = args$country,
    StateCode    = if ("StateCode" %in% names(latlon)) rep(latlon$StateCode, each = nrow(combination)) else NA,
    County       = if ("County" %in% names(latlon)) rep(latlon$County, each = nrow(combination)) else NA,
    Longitude    = if ("Longitude" %in% names(latlon)) rep(latlon$Longitude, each = nrow(combination)) else NA,
    Latitude     = if ("Latitude" %in% names(latlon)) rep(latlon$Latitude, each = nrow(combination)) else NA,
    ConstructionCodeType = if ("ConstructionCodeType" %in% names(combination)) rep(combination$ConstructionCodeType, nrow(latlon)) else NA,
    ConstructionCode     = if ("ConstructionCode" %in% names(combination)) rep(combination$ConstructionCode, nrow(latlon)) else NA,
    OccupancyCodeType    = if ("OccupancyCodeType" %in% names(combination)) rep(combination$OccupancyCodeType, nrow(latlon)) else NA,
    OccupancyCode        = if ("OccupancyCode" %in% names(combination)) rep(combination$OccupancyCode, nrow(latlon)) else NA,
    YearBuilt            = if ("YearBuilt" %in% names(combination)) rep(combination$YearBuilt, nrow(latlon)) else NA,
    Stories              = if ("Stories" %in% names(combination)) rep(combination$Stories, nrow(latlon)) else NA,
    GrossArea            = if ("GrossArea" %in% names(combination)) rep(combination$GrossArea, nrow(latlon)) else NA,
    cv_cols,
    UDF1        = rep(combination$USERID1, nrow(latlon)),
    UDF2        = rep(combination$USERID2, nrow(latlon)),
    check.names = FALSE
  )
  
  accntnum <- unique(paste0(accountsuffix, "_", combination$Classification))
  inceptdate <- paste0("1/1/", format(Sys.Date(), "%Y"))
  expiredate <- paste0("12/31/", format(Sys.Date(), "%Y"))
  acc_comb <- data.frame(
    PolicyID       = accntnum,
    InsuredName    = short,
    InceptionDate  = inceptdate,
    ExpirationDate = expiredate,
    Perils         = meta$code,
    LayerID        = 1,
    LayerPerils    = meta$code,
    Limit1         = 0,
    check.names = FALSE
  )
  
  list(loc_comb = loc_comb, acc_comb = acc_comb, locfilename = fn$locfilename, accfilename = fn$accfilename)
}

build_verisk_secmod <- function(latlon, combination, args) {
  validate_required_cols(latlon, c("LOCNAME"), "Location")
  validate_required_cols(combination, c("Classification", "Description"), "Combination")
  
  meta <- vendor_meta("Verisk", args$peril)
  short <- module_short(args$module)
  accountsuffix <- paste0(short, "_", args$subperil, meta$abb, args$suffix)
  fn <- build_output_names("Verisk", args$version, args$module, args$country, args$peril, args$subperil)
  
  rep_n <- nrow(latlon)
  n <- nrow(latlon) * nrow(combination)
  secmod_df <- expand_secmods(combination, rep_n)
  
  cv_cols <- data.frame(
    BuildingValue    = rep(args$bldgVal, n),
    ContentsValue    = rep(args$cntVal, n),
    TimeElementValue = rep(args$BIVal, n),
    check.names = FALSE
  )
  cv_cols <- cv_cols[, colSums(cv_cols != 0) > 0, drop = FALSE]
  
  loc_comb <- data.frame(
    LocationID   = seq_len(n),
    LocationName = rep(latlon$LOCNAME, each = nrow(combination)),
    PolicyID     = rep(paste0(accountsuffix, "_", combination$Classification), nrow(latlon)),
    CountryCode  = args$country,
    StateCode    = if ("StateCode" %in% names(latlon)) rep(latlon$StateCode, each = nrow(combination)) else NA,
    County       = if ("County" %in% names(latlon)) rep(latlon$County, each = nrow(combination)) else NA,
    Longitude    = if ("Longitude" %in% names(latlon)) rep(latlon$Longitude, each = nrow(combination)) else NA,
    Latitude     = if ("Latitude" %in% names(latlon)) rep(latlon$Latitude, each = nrow(combination)) else NA,
    cv_cols,
    secmod_df,
    UDF1        = rep(combination[["Description"]], nrow(latlon)),
    check.names = FALSE
  )
  
  accntnum <- unique(paste0(accountsuffix, "_", combination$Classification))
  inceptdate <- paste0("1/1/", format(Sys.Date(), "%Y"))
  expiredate <- paste0("12/31/", format(Sys.Date(), "%Y"))
  acc_comb <- data.frame(
    PolicyID       = accntnum,
    InsuredName    = short,
    InceptionDate  = inceptdate,
    ExpirationDate = expiredate,
    Perils         = meta$code,
    LayerID        = 1,
    LayerPerils    = meta$code,
    Limit1         = 0,
    check.names = FALSE
  )
  
  list(loc_comb = loc_comb, acc_comb = acc_comb, locfilename = fn$locfilename, accfilename = fn$accfilename)
}

build_files <- function(vendor, module, version, country, peril, subperil, suffix,
                        bldgVal, cntVal, BIVal, latlon, combination) {
  args <- list(
    vendor = vendor, module = module, version = version, country = country,
    peril = peril, subperil = subperil, suffix = suffix,
    bldgVal = bldgVal, cntVal = cntVal, BIVal = BIVal
  )
  
  if (!peril %in% names(peril_lookup())) stop("Invalid peril selected.")
  if (!subperil %in% peril_lookup()[[peril]]) stop("Invalid subperil selected.")
  
  if (vendor == "Moodys" && module == "Vulnerability Sensitivity") {
    return(build_moodys_vulnerability(latlon, combination, args))
  }
  if (vendor == "Moodys" && module == "Secondary Modifiers") {
    return(build_moodys_secmod(latlon, combination, args))
  }
  if (vendor == "Verisk" && module == "Vulnerability Sensitivity") {
    return(build_verisk_vulnerability(latlon, combination, args))
  }
  build_verisk_secmod(latlon, combination, args)
}