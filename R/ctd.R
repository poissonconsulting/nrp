#' Read CTD File
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return A tibble
#' @export
#'
nrp_read_ctd_file <- function(path, db_path = getOption("nrp.db_path", NULL)) {
  check_file_exists(path)

  if(!inherits(db_path, "SQLiteConnection")){
    db_path <- connect_if_valid_path(path = db_path)
    on.exit(readwritesqlite::rws_close_connection(conn = db_path))
  }
################################################################################
  suppressWarnings(
    ctd <- try(read.ctd.sbe(path, type="SBE19plus"), silent = TRUE))

  if(!inherits(ctd, "try-error")){
    data <- as_tibble(ctd@data)

    if(!"frequency" %in% names(data)){
      data$frequency <- NA_real_
    }

    units_list <- ctd@metadata$units
    meta_units <- extract_units(units_list)
    data %<>% map2_dfc(meta_units, fill_units)

    sites <- nrp_load_ctd_sites(db_path = db_path)

    siteIDs <- sites$SiteID
    match <- which(sapply(siteIDs, grepl, path, ignore.case = TRUE))

    if(length(match) == 0){
      err("Station name could not be extracted from file name: No matches")
    } else if(length(match) > 1){
      err("Station name could not be extracted from file name: More than one match")
    }
    data$SiteID <- siteIDs[match]


    colnames(data) %<>% str_to_title()
    data$DateTime <- ctd@metadata$startTime
    data$DateTime %<>% dttr::dtt_set_tz("Etc/GMT+8")

    data %<>% rename(SiteID = .data$Siteid) %>%
      select(.data$SiteID, .data$DateTime, everything())

  } else {

    col_names <- c("Depth","Temperature","Oxygen","Oxygen2",
                   "Conductivity","Conductivity2","Salinity","Backscatter","Fluorescence","Flag")

    data <- utils::read.table(file = path, col.names = col_names, skip = 100)
    data$Pressure <- NA_real_
    data$Frequency <- NA_real_

    lookup <- nrp::site_date_lookup
    data$DateTime <- lookup$Date[lookup$File == basename(path)]
    data$SiteID <- lookup$SiteID[lookup$File == basename(path)]

    data %<>% select(.data$SiteID, .data$DateTime, .data$Depth, .data$Temperature, .data$Oxygen,
                     .data$Oxygen2, .data$Conductivity, .data$Conductivity2, .data$Salinity,
                     .data$Backscatter, .data$Fluorescence, .data$Frequency, .data$Flag, .data$Pressure)
    data$DateTime %<>% as.POSIXct(tz = "Etc/GMT+8")
  }


############################################################################

  if(!nrow(distinct(data, .data$Depth)) == nrow(data)){
    n_removed <- nrow(data) - nrow(distinct(data, .data$Depth))
    message(paste(n_removed, "out of", nrow(data),  "duplicate depth readings removed from file",
                  path))
    data %<>% distinct(.data$Depth, .keep_all = TRUE)
  }

  default_units <- c(NA, NA, "m", "degree * C", "mg/l", "percent", "uS/cm", "mu * S/cm", "PSU", "NTU", "ug/L", "Hz", NA, "dbar")
  data %<>% map2_dfc(default_units, fill_units)

  check_ctd_data(data, exclusive = TRUE, order = TRUE)

  data
}

#' Read CTD Files
#'
#' @param path A string of the path to the directory.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @inheritParams fs::dir_ls
#' @return A list of tibbles.
#' @export
#'
nrp_read_ctd <- function(path = ".", db_path = getOption("nrp.db_path", NULL), recursive = FALSE, regexp = "[.]cnv$",
                         fail = TRUE) {
  check_dir_exists(path)
  paths <- dir_ls(path, type = "file", recursive = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_ctd_file(., db_path = db_path))))

  datas
}

#' Load CTD site table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return CTD site table
#' @export
#'
nrp_load_ctd_sites <- function(db_path = getOption("nrp.db_path", NULL)) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }

  site <- readwritesqlite::rws_read_sqlite_table("Sites", conn = conn)
  site
}

#' Add new ctd sites to database site table
#' @param data a tibble or data frame of new site data
#' Must have columns "SiteID", "SiteNumber", "SiteName", "BasinArm", "Depth", as well as columns
#' "easting" and "northing" with coordinates in projection UTM zone 11N.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @export
#'
nrp_add_ctd_sites <- function(data, db_path){

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }

  data %<>% poisspatial::ps_coords_to_sfc(coords = c("Easting", "Northing"), crs = 26911) %>%
    mutate(Depth = units::set_units(.data$Depth, "m"))

  readwritesqlite::rws_write_sqlite(x = data, commit = TRUE, strict = TRUE, silent = TRUE,
                                    x_name = "Sites", conn = conn)
}

#' Load CTD data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param db_path The SQLite connection object or path to the SQLite database
#'
#' @return CTD data table
#' @export
#'
nrp_load_ctd <- function(start_date = NULL, end_date = NULL, sites = NULL, db_path = getOption("nrp.db_path", NULL)){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }

  data <- tbl(conn, "CTD")

  if(is.null(start_date) || is.null(start_date)){
    span <- DBI::dbGetQuery(conn, "SELECT MIN(DateTime) AS Start, MAX(DateTime) AS End FROM CTD")
  }
  if(is.null(start_date)){
    start_date <- span$Start
  } else {
    checkr::check_datetime(as.POSIXct(start_date))
    start_date %<>% as.character()
  }
  if(is.null(end_date)){
    end_date <- span$End
  } else {
    checkr::check_datetime(as.POSIXct(end_date))
    end_date %<>% as.character()
  }
  site_table <- nrp_load_ctd_sites(db_path = conn)
  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }

  DateTime <- NULL
  SiteID <- NULL
  query <- data %>%
    filter(DateTime >= start_date, DateTime <= end_date, SiteID %in% sites)
  result <- query %>% dplyr::collect() %>%
    dplyr::mutate(DateTime = as.POSIXct(DateTime, origin = "1970-01-01", tz = "Etc/GMT+8"))
  result
}
