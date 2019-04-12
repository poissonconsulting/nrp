#' Read CTD File
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @param lookup The lookup table for assigning site names/dates (used when reading files that cannot be read by oce package).
#' this defaults to a dataset provided with the package that is used for reading historical data
#' @return A tibble
#' @export
nrp_read_ctd_file <- function(path, db_path = getOption("nrp.db_path", NULL),
                              lookup = nrp::site_date_lookup) {
  check_file_exists(path)
  check_site_date_lookup(data = lookup)

  if(!inherits(db_path, "SQLiteConnection")){
    db_path <- connect_if_valid_path(path = db_path)
    on.exit(readwritesqlite::rws_disconnect(conn = db_path))
  }

  suppressWarnings(
    ctd <- try(read.ctd.sbe(path, type="SBE19plus"), silent = TRUE))

  if(!inherits(ctd, "try-error")){
    data <- as_tibble(ctd@data)

    units_list <- ctd@metadata$units
    meta_units <- extract_units(units_list)
    data %<>% map2_dfc(meta_units, fill_units)

    if(!"frequency" %in% names(data)){
      data$frequency <- NA_real_
    }

    sites <- nrp_download_ctd_sites(db_path = db_path)

    siteIDs <- sites$SiteID
    match <- which(sapply(siteIDs, grepl, path, ignore.case = TRUE))

    if(length(match) == 1){
      data$SiteID <- siteIDs[match]
    } else if(basename(path) %in% lookup$File) {
      data$SiteID <- lookup$SiteID[lookup$File == basename(path)]
    } else {
      err("Station name could not be extracted from file name: No matches")
    }

    colnames(data) %<>% str_to_title()
    data$DateTime <- ctd@metadata$startTime
    data$DateTime %<>% dttr::dtt_set_tz("Etc/GMT+8")

    data %<>% rename(SiteID = .data$Siteid) %>%
      select(.data$SiteID, .data$DateTime, everything())

  } else {

    col_names <- c("Depth","Temperature","Oxygen","Oxygen2",
                   "Conductivity","Conductivity2","Salinity","Backscatter",
                   "Fluorescence","Flag")

    data <- utils::read.table(file = path, col.names = col_names, skip = 100)
    data$Pressure <- NA_real_
    data$Frequency <- NA_real_

    lookup <- site_date_lookup
    data$DateTime <- lookup$Date[lookup$File == basename(path)]
    data$SiteID <- lookup$SiteID[lookup$File == basename(path)]
  }

  n_pre_filt <- nrow(data)
  data %<>% filter(as.numeric(.data$Depth) >= 0)
  n_dups <- n_pre_filt - nrow(data)

  if(n_dups > 0){
    message(paste(n_dups, "negative depths removed from data"))
  }

  data %<>% mutate(Retain = if_else(duplicated(.data$Depth, fromLast = TRUE), FALSE, TRUE),
                   FileID = 1:nrow(data), File = basename(path))

  data %<>% select(.data$FileID, .data$SiteID, .data$DateTime, .data$Depth,
                   .data$Temperature, .data$Oxygen,
                   .data$Oxygen2, .data$Conductivity, .data$Conductivity2,
                   .data$Salinity,
                   .data$Backscatter, .data$Fluorescence, .data$Frequency,
                   .data$Flag, .data$Pressure,
                   .data$Retain, .data$File)

  default_units <- c(NA, NA, NA, "m", "degC", "mg/l", "percent", "uS/cm",
                     "mu * S/cm", "PSU",
                     "NTU", "ug/L", "Hz", NA, "dbar", NA, NA)
  data %<>% map2_dfc(default_units, fill_units)
  units(data$Temperature) <- NULL
  units(data$Temperature) <- "degC"

  data$DateTime %<>% as.POSIXct(tz = "Etc/GMT+8")
  data$Time <- dttr::dtt_time(data$DateTime, tz = "Etc/GMT+8")
  data$Time[data$Time == 00:00:00] <- NA
  data$Date <- dttr::dtt_date(data$DateTime)

  data %<>% select(.data$FileID, .data$SiteID, .data$Date, .data$Time,
                   everything(), -.data$DateTime)
  data
}


#' Read CTD Files
#'
#' @param path A string of the path to the directory.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @param lookup The lookup table for assigning site names/dates
#' (used when reading files that cannot be read by oce package).
#' this defaults to a dataset provided with the package that is used for
#' reading historical data
#' @inheritParams fs::dir_ls
#' @return A list of tibbles.
#' @export
#'
nrp_read_ctd <- function(path = ".", db_path = getOption("nrp.db_path", NULL),
                         recursive = FALSE, regexp = "[.]cnv$",
                         fail = TRUE, lookup = nrp::site_date_lookup) {
  check_dir_exists(path)
  paths <- dir_ls(path, type = "file", recursive = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_ctd_file(., db_path = db_path))))
  rownames(datas) <- NULL

  datas
}

#' Download CTD site table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return CTD site table
#' @export
#'
nrp_download_ctd_sites <- function(db_path = getOption("nrp.db_path", NULL)) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }
  readwritesqlite::rws_read_table("sitesCTD", conn = conn)
}


#' Download CTD visit table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return CTD visit table
#' @export
#'
nrp_download_ctd_visit <- function(db_path = getOption("nrp.db_path", NULL)) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }
  readwritesqlite::rws_read_table("VisitCTD", conn = conn)
}


#' Download CTD BasinArm table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return CTD BasinArm table
#' @export
#'
nrp_download_ctd_basin_arm <- function(db_path = getOption("nrp.db_path", NULL)) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  readwritesqlite::rws_read_table("BasinArm", conn = conn)
}

#' Add new ctd sites to database site table
#' @param data a tibble or data frame of new site data
#' Must have columns "SiteID", "SiteNumber", "SiteName", "BasinArm",
#' "Depth", as well as columns
#' "Easting" and "Northing" with coordinates in projection UTM zone 11N.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @export
#'
nrp_add_ctd_sites <- function(data, db_path = getOption("nrp.db_path", NULL)){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  data %<>% sf::st_as_sf(coords = c("Easting", "Northing"), crs = 26911) %>%
    mutate(MaxDepth = units::set_units(.data$MaxDepth, "m"))

  readwritesqlite::rws_write(x = data, commit = TRUE, strict = TRUE, silent = TRUE,
                             x_name = "sitesCTD", conn = conn)
}

#' Upload CTD data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
nrp_upload_ctd <- function(data, db_path = getOption("nrp.db_path", NULL),
                           commit = TRUE, strict = TRUE, silent = TRUE,
                           replace = FALSE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ctd_data(data, exclusive = TRUE, order = TRUE)

  visit <- group_by(data, .data$SiteID, .data$Date, .data$Time) %>%
    summarise(DepthDuplicates = length(which(.data$Retain == FALSE)),
              File = first(.data$File)) %>%
    ungroup()

  visit_db <- nrp_download_ctd_visit(db_path = conn)
  visit_upload <- setdiff(visit, visit_db)

  readwritesqlite::rws_write(x = visit_upload, commit = commit,
                             strict = strict, silent = silent,
                             x_name = "visitCTD", conn = conn)

  n_pre_filt <- nrow(data)
  data %<>% filter(.data$Retain == TRUE)
  n_dups <- n_pre_filt - nrow(data)
  message(paste(n_dups, "duplicate depths removed from data"))

  data %<>% select(-.data$File, -.data$Retain)

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict,
                             silent = silent,
                             x_name = "CTD", conn = conn, replace = replace)
}

#' Dowload CTD data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param parameters A character vector of the parameters to include.
#' Permissable values: "Temperature", "Oxygen", "Oxygen2", "Conductivity",
#' "Conductivity2", "Salinity", "Backscatter", "Fluorescence", "Frequency",
#' "Flag", "Pressure"
#' @param db_path The SQLite connection object or path to the SQLite database
#'
#' @return CTD data table
#' @export
#'
nrp_download_ctd <- function(start_date = "2018-01-01", end_date = "2018-12-31",
                             sites = NULL, parameters = "all",
                             db_path = getOption("nrp.db_path", NULL)){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  default_parameters <- c("Depth", "Temperature", "Oxygen", "Oxygen2",
                          "Conductivity","Conductivity2",
                          "Salinity", "Backscatter", "Fluorescence",
                          "Frequency", "Flag", "Pressure")
  if(start_date > end_date){
    err("start date is later than end date")
  }
  if(end_date > Sys.Date()){
    err("end date is later than present day")
  }

  checkr::check_datetime(as.POSIXct(start_date))
  start_date <- as.character(dttr::dtt_date(start_date))
  checkr::check_datetime(as.POSIXct(end_date))
  end_date <- as.character(dttr::dtt_date(end_date))

  site_table <- nrp_download_ctd_sites(db_path = conn)
  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }
  if(parameters == "all"){
    parameters <- default_parameters
  } else if(!all(parameters %in% default_parameters)){
    err(paste("1 or more invalid parameter names"))
  }

  parameters <-c("FileID", "SiteID", "Date", "Time", parameters)

  Date <- NULL
  SiteID <- NULL

  paramsSql <- cc(parameters, ellipsis = length(parameters) + 1, bracket = "`")
  sitesSql <- cc(sites, ellipsis = length(sites) + 1)
  start_dateSql <- paste0("'", start_date, "'")
  end_dateSql <- paste0("'", end_date, "'")

  query <- paste0("SELECT ", paramsSql, " FROM CTD WHERE ((`Date` >= ", start_dateSql, ") AND (`Date` <= ",
                  end_dateSql, ") AND (`SiteID` IN (", sitesSql,")))")

  readwritesqlite::rws_query(query = query, conn = conn, meta = TRUE) %>%
    dplyr::mutate(Date = dttr::dtt_date(.data$Date))
}
