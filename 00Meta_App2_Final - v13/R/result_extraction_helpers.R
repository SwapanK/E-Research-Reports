# Helper functions for Result Extraction App

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

vendor_tag <- function(vendor) if (vendor %in% c("Moodys", "Moody's", "RMS")) "RMS" else "Verisk"
module_tag <- function(module) if (grepl("Vulnerability", module, ignore.case = TRUE)) "Vulsens" else "Secmod"

default_server <- function(vendor, i = 1) {
  if (vendor_tag(vendor) == "RMS") return("76873233b05d6e018356bc5b418edbe6.databridge.rms-pe.com,1433")
  if (i == 1) "GREAZUS1DB851P" else "GREAZUS1DB801P"
}

default_version_label <- function(vendor, i = 1) {
  if (vendor_tag(vendor) == "RMS") return(if (i == 1) "RLv25" else "RLv23")
  if (i == 1) "v13" else "v12"
}

sample_paths_extract <- function(vendor, module) {
  base <- "data/result_extraction_sample/"
  v <- vendor_tag(vendor)
  m <- module_tag(module)
  if (v == "RMS" && m == "Vulsens") main <- paste0(base, "vulnerability_sensitivity_rms.csv")
  if (v == "RMS" && m == "Secmod")  main <- paste0(base, "secmod_RMS_USHU.csv")
  if (v == "Verisk" && m == "Vulsens") main <- paste0(base, "vulnerability_sensitivity_air.csv")
  if (v == "Verisk" && m == "Secmod")  main <- paste0(base, "secmod_AIR_USSCS.csv")
  region <- paste0(base, "region_statecode_file.csv.csv")
  list(main = main, region = region)
}

quote_db <- function(x) {
  x <- gsub("^\\[|\\]$", "", trimws(x))
  paste0("[", gsub("\\]", "]]", x), "]")
}

clean_id <- function(x, name = "ID") {
  x <- trimws(as.character(x %||% ""))
  if (!grepl("^[0-9]+$", x)) stop(name, " must be numeric and must exist in the selected ExposureSet/ResultSet.")
  x
}

safe_sql <- function(conn, query, context = "SQL query") {
  res <- RODBC::sqlQuery(conn, query, stringsAsFactors = FALSE)
  if (inherits(res, "try-error") || (is.character(res) && length(res) == 1 && grepl("ERROR", res, ignore.case = TRUE))) {
    stop(context, " failed. Details: ", paste(res, collapse = "\n"))
  }
  as.data.frame(res, stringsAsFactors = FALSE)
}

connect_sql <- function(vendor, server, user = NULL, password = NULL) {
  if (!nzchar(trimws(server))) stop("Server name is required.")
  if (vendor_tag(vendor) == "RMS") {
    if (!nzchar(trimws(user %||% "")) || !nzchar(password %||% "")) {
      stop("RMS connection requires username and password. Click Connect and enter credentials in the popup.")
    }
    cs <- glue::glue("DRIVER=SQL Server;server={server};uid={user};pwd={password};")
  } else {
    cs <- glue::glue("DRIVER=SQL Server;server={server};trusted_connection=true;")
  }
  RODBC::odbcDriverConnect(as.character(cs))
}

list_databases <- function(conn) {
  x <- safe_sql(conn, "SELECT name FROM sys.databases ORDER BY name", "Database list")
  x$name
}

get_exposure_set <- function(conn, vendor, database_edm) {
  db <- quote_db(database_edm)
  if (vendor_tag(vendor) == "RMS") {
    safe_sql(conn, paste0("SELECT * FROM ", db, ".dbo.portinfo"), "ExposureSet query")
  } else {
    safe_sql(conn, paste0("SELECT * FROM ", db, ".dbo.tExposureSet"), "ExposureSet query")
  }
}

get_result_set <- function(conn, vendor, database_rdm) {
  db <- quote_db(database_rdm)
  if (vendor_tag(vendor) == "RMS") {
    safe_sql(conn, paste0("SELECT * FROM ", db, ".dbo.rdm_analysis"), "ResultSet query")
  } else {
    safe_sql(conn, paste0("SELECT * FROM ", db, ".dbo.tAnalysisResult"), "ResultSet query")
  }
}

loc_rms <- function(conn, portinfoid, database_edm) {
  pid <- clean_id(portinfoid, "Portinfoid")
  db <- quote_db(database_edm)
  q <- paste0(
    "SELECT l.LOCNUM, l.LOCNAME, l.STATECODE, l.COUNTY, l.OCCSCHEME, l.OCCTYPE, l.BLDGSCHEME, ",
    "l.BLDGCLASS, l.NUMSTORIES, l.YEARBUILT, l.FLOORAREA, lc.TIV ",
    "FROM ", db, ".dbo.loc l JOIN (",
    "SELECT lc.locid, SUM(lc.valueamt) AS TIV FROM ", db, ".dbo.loccvg lc ",
    "JOIN ", db, ".dbo.loc l ON lc.locid = l.locid ",
    "WHERE ACCGRPID IN (SELECT ACCGRPID FROM ", db, ".dbo.portacct WHERE portinfoid = ", pid, ") ",
    "GROUP BY lc.LOCID) lc ON l.LOCID = lc.LOCID ORDER BY TRY_CAST(l.LOCNUM AS INT) ASC"
  )
  out <- safe_sql(conn, q, "RMS location extraction")
  if (!"LOCNUM" %in% names(out) || nrow(out) == 0) stop("Portinfoid provided does not exist or returned no locations.")
  out
}

aal_rms <- function(conn, anlsid, portinfoid, database_edm, database_rdm) {
  aid <- clean_id(anlsid, "Anlsid")
  pid <- clean_id(portinfoid, "Portinfoid")
  edm <- quote_db(database_edm)
  rdm <- quote_db(database_rdm)
  q <- paste0(
    "SELECT l.LOCNUM, l.LOCNAME, l.STATECODE, l.COUNTY, l.OCCSCHEME, l.OCCTYPE, l.BLDGSCHEME, ",
    "l.BLDGCLASS, l.FLOORAREA, rl.PUREPREMIUM AS AAL ",
    "FROM ", edm, ".dbo.loc l JOIN ", rdm, ".dbo.rdm_locstats rl ON rl.id = l.locid ",
    "WHERE ACCGRPID IN (SELECT ACCGRPID FROM ", edm, ".dbo.portacct WHERE portinfoid = ", pid, ") ",
    "AND rl.ANLSID = ", aid
  )
  out <- safe_sql(conn, q, "RMS result extraction")
  if (!"AAL" %in% names(out) || nrow(out) == 0) stop("Anlsid provided does not exist or returned no result rows.")
  out
}

loc_verisk <- function(conn, portinfoid, database_edm) {
  pid <- clean_id(portinfoid, "ExposureSetSID")
  db <- quote_db(database_edm)
  q <- paste0(
    "SELECT l.LocationID, l.LocationName, l.LocationSID, c.ContractID, l.AreaCode, ",
    "l.SubAreaName, l.PostalCode, l.LATITUDE, l.LONGITUDE, l.AIROccupancyCode AS OccupancyCode, ",
    "l.AIRConstructionCodeA AS ConstructionCode, l.YearBuilt, l.Stories, l.GrossArea ",
    "FROM ", db, ".dbo.tLocation AS l JOIN ", db, ".dbo.tContract AS c ON l.ContractSID = c.ContractSID ",
    "WHERE c.ExposureSetSID = ", pid
  )
  out <- safe_sql(conn, q, "Verisk location extraction")
  if (!"LocationID" %in% names(out) || nrow(out) == 0) stop("ExposureSetSID provided does not exist or returned no locations.")
  out
}

aal_verisk <- function(conn, anlsid, database_edm, database_rdm) {
  aid <- clean_id(anlsid, "Anlsid")
  edm <- quote_db(database_edm)
  rdm <- quote_db(database_rdm)
  q <- paste0(
    "SELECT l.LocationID, l.LocationName, l.AreaCode AS STATECODE, l.SubAreaName AS COUNTY, ",
    "l.AIROccupancyCode AS OccupancyCode, l.AIRConstructionCodeA AS ConstructionCode, ",
    "l.YearBuilt, l.Stories, rl.GroundUpLoss AS AAL ",
    "FROM ", rdm, ".dbo.t", aid, "_LOSS_ByLocationSummary AS rl ",
    "JOIN ", edm, ".dbo.tLocation AS l ON l.LocationSID = rl.LocationSID"
  )
  out <- safe_sql(conn, q, "Verisk result extraction")
  if (!"AAL" %in% names(out) || nrow(out) == 0) stop("Anlsid provided does not exist or returned no result rows.")
  out
}

read_input_csv <- function(path, expected_type = "input file") {
  if (is.null(path) || !file.exists(path)) stop(expected_type, " is missing.")
  data.table::fread(path, na.strings = c("", "NA", "NaN"), showProgress = FALSE)
}

validate_input_file <- function(dt, vendor, module) {
  required <- c("Classification", "Description")
  miss <- setdiff(required, names(dt))
  if (length(miss)) stop("Input file not in correct format, see sample files. Missing columns: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

load_region_file <- function(path = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  r <- data.table::fread(path, showProgress = FALSE)
  nms <- names(r)
  state_col <- nms[tolower(nms) %in% c("statecode", "state", "iso2dig_id", "iso2")][1]
  region_col <- nms[tolower(nms) %in% c("region", "regionname", "rms_region")][1]
  if (is.na(state_col) || is.na(region_col)) stop("Region file is not in correct format. It needs a state code column and a region column, for example STATECODE and Region.")
  r <- as.data.frame(r[, .SD, .SDcols = c(state_col, region_col)])
  names(r) <- c("STATECODE", "Region")
  r$STATECODE <- as.character(r$STATECODE)
  r
}

attach_template <- function(loc, template) {
  loc$Classification <- rep(template$Classification, length.out = nrow(loc))
  loc$Description <- rep(template$Description, length.out = nrow(loc))
  loc
}

build_identifier <- function(loc, vendor) {
  if (vendor_tag(vendor) == "RMS") {
    yb <- suppressWarnings(as.numeric(format(as.Date(loc$YEARBUILT), "%Y")))
    yb[is.na(yb)] <- suppressWarnings(as.numeric(loc$YEARBUILT[is.na(yb)]))
    paste0(loc$BLDGSCHEME, loc$BLDGCLASS, "_", loc$OCCSCHEME, loc$OCCTYPE,
           "_YB", yb, "_HT", loc$NUMSTORIES, "_FA", loc$FLOORAREA,
           "_", loc$Classification)
  } else {
    ga <- loc$GrossArea %||% 0
    paste0(loc$ConstructionCode, "_", loc$OccupancyCode, "_", loc$Stories, "_",
           loc$YearBuilt, "_", ga, "_", loc$Classification)
  }
}

summarise_by <- function(dt, by_cols, value_cols) {
  data.table::setDT(dt)
  dt[, lapply(.SD, function(x) stats::median(as.numeric(x), na.rm = TRUE)), by = by_cols, .SDcols = value_cols]
}

make_outputs <- function(final_comp, vendor, region_dt = NULL) {
  data.table::setDT(final_comp)
  value_cols <- setdiff(names(final_comp), c("LOCID", "LOCNAME", "STATECODE", "COUNTY", "Region", "CC_OCC_HT_YB_FA", "CC_OCC_HT_YT_FA", "Classification", "Description"))
  id_col <- intersect(c("CC_OCC_HT_YB_FA", "CC_OCC_HT_YT_FA"), names(final_comp))[1]
  if (!"COUNTY" %in% names(final_comp)) final_comp[, COUNTY := NA_character_]
  if (!is.null(region_dt)) final_comp[, Region := region_dt$Region[match(as.character(STATECODE), as.character(region_dt$STATECODE))]]
  state <- summarise_by(copy(final_comp), c("STATECODE", "Classification", "Description", id_col), value_cols)
  county <- summarise_by(copy(final_comp), c("STATECODE", "COUNTY", "Classification", "Description", id_col), value_cols)
  out <- list(byLocationfile = as.data.frame(final_comp), byStatefile = as.data.frame(state), byCounty = as.data.frame(county))
  if (!is.null(region_dt) && "Region" %in% names(final_comp)) {
    region <- summarise_by(copy(final_comp)[!is.na(Region)], c("Region", "Classification", "Description", id_col), value_cols)
    out$byRegion <- as.data.frame(region)
  }
  out
}

output_filenames <- function(country, vendor, module, peril, subperil, suffix) {
  base <- paste(vendor_tag(vendor), module_tag(module), country, peril, subperil, suffix, sep = "_")
  list(
    byLocationfile = paste0(base, "_byLocationfile.csv"),
    byStatefile    = paste0(base, "_byStatefile.csv"),
    byCounty       = paste0(base, "_byCounty.csv"),
    byRegion       = paste0(base, "_byRegion.csv")
  )
}

run_extraction <- function(vendor, module, template, region_dt, versions, country, peril, subperil, suffix) {
  validate_input_file(template, vendor, module)
  vtag <- vendor_tag(vendor)
  mtag <- module_tag(module)
  if (mtag == "Secmod" && length(versions) != 1) stop("Secondary Modifiers module supports only one version.")
  if (mtag == "Vulsens" && !(length(versions) %in% c(1, 2))) stop("Vulnerability Sensitivity requires one or two versions.")
  
  first <- versions[[1]]
  loc <- if (vtag == "RMS") loc_rms(first$conn, first$portinfoid, first$edm_db) else loc_verisk(first$conn, first$portinfoid, first$edm_db)
  key <- if (vtag == "RMS") "LOCNUM" else "LocationID"
  loc <- attach_template(loc, template)
  id_col <- if (vtag == "RMS") "CC_OCC_HT_YB_FA" else "CC_OCC_HT_YT_FA"
  loc[[id_col]] <- build_identifier(loc, vendor)
  final <- data.table::as.data.table(loc)
  
  raw_labels <- vapply(versions, function(x) trimws(x$label %||% "Version"), character(1))
  blank_labels <- !nzchar(raw_labels)
  raw_labels[blank_labels] <- paste0("Version", seq_along(raw_labels))[blank_labels]
  value_cols <- make.names(raw_labels, unique = TRUE)
  
  for (i in seq_along(versions)) {
    v <- versions[[i]]
    losses <- if (vtag == "RMS") aal_rms(v$conn, v$anlsid, v$portinfoid, v$edm_db, v$rdm_db) else aal_verisk(v$conn, v$anlsid, v$edm_db, v$rdm_db)
    final[[value_cols[i]]] <- losses$AAL[match(final[[key]], losses[[key]])]
  }
  
  if (vtag == "RMS") {
    keep <- c("LOCNUM", "LOCNAME", "STATECODE", "COUNTY", "CC_OCC_HT_YB_FA", "Classification", "Description", value_cols)
    final_comp <- final[, ..keep]
    data.table::setnames(final_comp, "LOCNUM", "LOCID")
  } else {
    keep <- c("LocationID", "LocationName", "AreaCode", "SubAreaName", "CC_OCC_HT_YT_FA", "Classification", "Description", value_cols)
    final_comp <- final[, ..keep]
    data.table::setnames(final_comp, c("LocationID", "LocationName", "AreaCode", "SubAreaName"), c("LOCID", "LOCNAME", "STATECODE", "COUNTY"))
  }
  
  outputs <- make_outputs(final_comp, vendor, region_dt)
  list(outputs = outputs, filenames = output_filenames(country, vendor, module, peril, subperil, suffix))
}
