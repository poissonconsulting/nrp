#' Read mysid raw data file
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @param system The system 'arrow' or 'kootenay'. If null, the system is detected from the file name.
#' @return A tibble
#' @export
#'
nrp_read_mysid_file <- function(path, db_path = getOption("nrp.db_path",
                                                          file.choose()), system = NULL) {
  check_file_exists(path)
  chk::chk_null_or(system, chk = chk::chk_chr)

  if(is.null(system)){
    if(str_detect(tolower(basename(path)), "^ar")){
      system <- "AR"
    } else if(str_detect(tolower(basename(path)), "^kl")) {
      system <- "KL"
    } else {
      err("'system' cannot be detected from file name. Please set system argument to 'arrow' or 'kootenay'.")
    }
  } else {
    system <- tolower(system)
    if(!system %in% c("arrow", "kootenay")) err("'system' must be one of 'arrow', 'kootenay'.")
    system <- ifelse(system == "arrow", "AR", "KL")
  }

  if(!inherits(db_path, "SQLiteConnection")){
    db_path <- connect_if_valid_path(path = db_path)
    on.exit(readwritesqlite::rws_disconnect(conn = db_path))
  }

  data <- try(readxl::read_excel(path, col_types = "text"), silent = TRUE)

  if(inherits(data, "try-error")){
    err("Please ensure input data is a valid excel spreadsheet (.xlsx).")
  }

  data %<>% filter(!all_na(data))

  chk::check_key(data, key = c("Date", "Station", "Replicate"))

  data %<>%
    mutate(Station = as.integer(str_extract(.data$Station, "\\d"))) %>%
    clean_input_cols(lookup = nrp::mysid_input_cols) %>%
    mutate(Station = paste0(system, .data$Station))

  sites <- nrp_download_sites(db_path = db_path)

  if(!all(unique(data$Station) %in% sites$SiteID)) warning("Sites present in input data that are not found in 'Sites' table in database.")

  data
}

#' Read mysid raw data files
#'
#' @param path A string of the path to the directory.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @param system The system 'arrow' or 'kootenay'. If null, the system is detected from the file names.
#' @inheritParams fs::dir_ls
#' @return A tibble.
#' @export
#'
nrp_read_mysid <- function(path = ".", db_path = getOption("nrp.db_path", file.choose()),
                                 recursive = FALSE, system = NULL, regexp = "[.]xlsx$",
                                 fail = TRUE) {

  check_dir_exists(path)
  chk::chk_null_or(system, chk = chk::chk_character)
  chk::chk_chr(regexp)
  chk::chk_flag(recursive)
  chk::chk_flag(fail)

  paths <- dir_ls(path, type = "file", recurse = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_mysid_file(., db_path = db_path, system = system))))
  rownames(datas) <- NULL

  datas
}

#' Upload mysid data to the nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An SQLite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
nrp_upload_mysid <- function(data, db_path = getOption("nrp.db_path", file.choose()),
                             commit = TRUE, strict = TRUE, silent = TRUE,
                             replace = FALSE){
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_mysid_raw_data(data, exclusive = TRUE, order = TRUE)

  data %<>% mutate(Date = dttr2::dtt_date(.data$Date),
                   Depth = units::as_units(.data$Depth, "m"),
                   DepthCat = factor(.data$DepthCat, levels = c("Shallow", "Deep")))

  mysid_sample <- select(data, c(.data$Date, SiteID = .data$Station, .data$Replicate, .data$FileName,
                                 .data$MonthCat, .data$Time, .data$Depth, .data$DepthCat,
                                 .data$SideLake, SplMade = .data$`#splitsMade`,
                                 SplCount = .data$`#splitsCounted`, .data$FundingSource,
                                 .data$FieldCollection, .data$Analyst, .data$Comment))

  readwritesqlite::rws_write(x = mysid_sample, commit = commit,
                             strict = strict, silent = silent,
                             x_name = "MysidSample", conn = conn, replace = replace)

  mysid_data <- select(data, c(.data$Date, SiteID = .data$Station, .data$Replicate, .data$DenTotal,
                               .data$Djuv, .data$DimmM, .data$DmatM, .data$DbreedM,
                               .data$DimmF, .data$DmatF, .data$DbroodF, .data$DspentF,
                               .data$DdistBrF, .data$BiomTotal, .data$Bjuv, .data$BimmM,
                               .data$BmatM, .data$BbreedM, .data$BimmF, .data$BmatF,
                               .data$BbroodF, .data$BspentF, .data$BdistBrF, .data$VolDenTotal,
                               .data$VolDjuv, .data$VolDimmM, .data$VolDmatM, .data$VolDbreedM,
                               .data$VolDimmF, .data$VolDmatF, .data$VolDbroodF, .data$VolDspentF,
                               .data$VolDdisBrF, .data$`Eggs/BroodF`, .data$`Eggs/DistBrF`,
                               .data$`Eggs/Total#Mysids`, .data$PropFemGravid)) %>%
    tidyr::pivot_longer(cols = -c(.data$Date, .data$SiteID, .data$Replicate), names_to = "Parameter", values_to = "Value")

  readwritesqlite::rws_write(x = mysid_data, commit = commit, strict = strict,
                             silent = silent,
                             x_name = "Mysid", conn = conn, replace = replace)
}

#' Download Mysid sample table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return Mysid sample table
#' @export
#'
nrp_download_mysid_sample <- function(db_path = getOption("nrp.db_path", file.choose())) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }
  readwritesqlite::rws_read_table("MysidSample", conn = conn)
}


#' Download Mysid data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param parameters A character vector of the parameters to include.
#' Permissible values can be found in the nrp::mysid_params
#' @param db_path The SQLite connection object or path to the SQLite database
#'
#' @return CTD data table
#' @export
#'
nrp_download_mysid <- function(start_date = NULL, end_date = NULL,
                               sites = NULL, parameters = "all",
                               db_path = getOption("nrp.db_path", file.choose())){

  chk::chk_null_or(sites, chk = chk::chk_character)
  chk::chk_character(parameters)
  chk::chk_null_or(start_date, chk = check_chr_date)
  if(!is.null(start_date)) {
    check_chr_date(end_date)
  }
   conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  site_table <- nrp_download_sites(db_path = conn)
  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }

  if(parameters == "all"){
    parameters <- nrp::mysid_params
  } else if(!all(parameters %in% nrp::mysid_params)){
    err(paste("1 or more invalid parameter names"))
  }

  dates <- fill_date_query(table = "Mysid", col = "Date", end = end_date, start = start_date,
                           connection = conn)
  start_date <- dates["start_date"][[1]]
  end_date <- dates["end_date"][[1]]

  if(start_date > end_date){
    err("start date is later than end date")
  }

  Date <- NULL
  SiteID <- NULL
  paramsSql <- cc(parameters, ellipsis = 1000)
  sitesSql <- cc(sites, ellipsis = 1000)
  start_dateSql <- paste0("'", start_date, "'")
  end_dateSql <- paste0("'", end_date, "'")
  colsSql <- cc(c("Date", "SiteID", "Replicate", "Parameter", "Value"),
                ellipsis = 1000, brac = "`")

  query <- paste0("SELECT", colsSql, "FROM Mysid WHERE ((`Date` >= ", start_dateSql, ") AND (`Date` <= ",
                  end_dateSql, ") AND (`SiteID` IN (", sitesSql,")) AND (`Parameter` IN (", paramsSql,")))")

  result <- readwritesqlite::rws_query(query = query, conn = conn, meta = TRUE) %>%
    dplyr::mutate(Date = dttr2::dtt_date(.data$Date))

  if(nrow(result) == 0) warning("no data available for query provided.")

  result %<>% tidyr::pivot_wider(names_from = "Parameter", values_from = "Value")
  result
}
