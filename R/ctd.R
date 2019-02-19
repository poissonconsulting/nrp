#' Read CTD File
#'
#' @param path A string of the path to the file.
#' @return A tibble
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
#'              package = "nrp")
#' nrp_read_ctd_file(path)
nrp_read_ctd_file <- function(path) {
  check_file_exists(path)

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

  sites <- nrp_load_ctd_sites()

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
  data %<>% mutate(Depth = units::set_units(.data$Depth, "m"),
                   Temperature = units::set_units(.data$Temperature, "degree * C"),
                   Oxygen = units::set_units(.data$Oxygen, "mg/l"),
                   Oxygen2 = units::set_units(.data$Oxygen2, "%"),
                   Conductivity = units::set_units(.data$Conductivity, "uS/cm"),
                   Conductivity2 = units::set_units(.data$Conductivity2, "mu * S/cm"),
                   Salinity = units::set_units(.data$Salinity, "PSU"),
                   Backscatter = units::set_units(.data$Backscatter, "NTU"),
                   Fluorescence = units::set_units(.data$Fluorescence, "ug/L"),
                   Frequency = units::set_units(.data$Frequency, "Hz"),
                   Pressure = units::set_units(.data$Pressure, "dbar"))

  check_ctd_data(data, exclusive = TRUE, order = TRUE)

  data
}

#' Read CTD Files
#'
#' @param path A string of the path to the directory.
#' @inheritParams fs::dir_ls
#' @return A list of tibbles.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ctd/2018", package = "nrp")
#' nrp_read_ctd(path)
nrp_read_ctd <- function(path = ".", recursive = FALSE, regexp = "[.]cnv$",
                         fail = TRUE) {
  check_dir_exists(path)
  paths <- dir_ls(path, type = "file", recursive = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(purrr::map_dfr(paths, nrp_read_ctd_file))
  datas %<>% mutate(Depth = units::set_units(.data$Depth, "m"),
                    Temperature = units::set_units(.data$Temperature, "degree * C"),
                    Oxygen = units::set_units(.data$Oxygen, "mg/l"),
                    Oxygen2 = units::set_units(.data$Oxygen2, "%"),
                    Conductivity = units::set_units(.data$Conductivity, "uS/cm"),
                    Conductivity2 = units::set_units(.data$Conductivity2, "mu * S/cm"),
                    Salinity = units::set_units(.data$Salinity, "PSU"),
                    Backscatter = units::set_units(.data$Backscatter, "NTU"),
                    Fluorescence = units::set_units(.data$Fluorescence, "ug/L"),
                    Frequency = units::set_units(.data$Frequency, "Hz"),
                    Pressure = units::set_units(.data$Pressure, "dbar"))
  datas
}

#' Load CTD site table
#'
#' @return CTD site table
#' @export
#'
#' @examples
#' sites <- nrp_load_ctd_sites()

nrp_load_ctd_sites <- function() {

  db_path <-  system.file("extdata", "database_template/nrp.sqlite",
                          package = "nrp", mustWork = TRUE)

  conn <- suppressWarnings(readwritesqlite::rws_open_connection(dbname = db_path,  exists = TRUE))
  site <- readwritesqlite::rws_read_sqlite_table("Site", conn = conn)
  readwritesqlite::rws_close_connection(conn = conn)
  site
}

#' Load CTD data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param conn The SQLite connection object or path to the SQLite database
#'
#' @return CTD data table
#' @export

nrp_load_ctd <- function(start_date = NULL, end_date = NULL, sites = NULL, conn){

  if(!class(conn) == "SQLiteConnection"){
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
    # end_date %<>% as.POSIXct(tz = "Etc/GMT+8") %>% as.numeric
  }
  site_table <- nrp_load_ctd_sites()
  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }

  DateTime <- NULL
  SiteID <- NULL
  query <- data %>%
    filter(DateTime >= start_date, DateTime <= end_date, SiteID %in% sites) %>%
    show_query()
  result <- query %>% dplyr::collect() %>%
    dplyr::mutate(DateTime = as.POSIXct(DateTime, origin = "1970-01-01", tz = "Etc/GMT+8"))
  result
}
