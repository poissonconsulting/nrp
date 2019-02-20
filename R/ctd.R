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

  withCallingHandlers(
    ctd <- read.ctd.sbe(path, type="SBE19plus"),
    warning=function(w) {
      if (str_detect(w$message, "created 'pressure' from 'depth'"))
        invokeRestart("muffleWarning")
    })

  data <- as_tibble(ctd@data)

  if(!nrow(distinct(data, .data$depth)) == nrow(data)){
    n_removed <- nrow(data) - nrow(distinct(data, .data$depth))
    message(paste(n_removed, "out of", nrow(data),  "duplicate depth readings removed from file",
                  path))
    data %<>% distinct(.data$depth, .keep_all = TRUE)
  }

  sites <- nrp_load_ctd_sites(db_path = db_path)

  siteIDs <- sites$SiteID

  match <- which(sapply(siteIDs, grepl, path))
  if(length(match) == 0){
    err("Station name could not be extracted from file name: No matches")
  } else if(length(match) > 1){
    err("Station name could not be extracted from file name: More than one match")
  }

  # we will need to get units from metadata
  # note we may need to update check_ctd_data accordingly
  colnames(data) %<>% str_to_title()
  data$DateTime <- ctd@metadata$startTime
  data$DateTime %<>% dttr::dtt_set_tz("Etc/GMT+8")
  data$SiteID <- siteIDs[match]
  data %<>% select(.data$SiteID, .data$DateTime, everything())

  data %<>% mutate(Flag = as.logical(.data$Flag))
  default_units <- c(NA, NA, "m", "degree * C", "mg/l", "%", "uS/cm", "mu * S/cm", "PSU", "NTU", "ug/L", "Hz", NA, "dbar")
  data <- map2_dfc(data, default_units, fill_units)
  # i will remove this hack once chacks and db are changed to accept logical
  data %<>% mutate(Flag = as.numeric(.data$Flag))

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


  datas <- suppressWarnings(purrr::map_dfr(paths, ~ nrp_read_ctd_file(., db_path = db_path)))

  datas %<>% mutate(Flag = as.logical(.data$Flag))
  default_units <- c(NA, NA, "m", "degree * C", "mg/l", "%", "uS/cm", "mu * S/cm", "PSU", "NTU", "ug/L", "Hz", NA, "dbar")
  datas <- map2_dfc(datas, default_units, fill_units)
  # i will remove this hack once chacks and db are changed to accept logical
  datas %<>% mutate(Flag = as.numeric(.data$Flag))

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
