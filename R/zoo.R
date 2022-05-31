#' Read zooplankton raw data file
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @param system The system 'arrow' or 'kootenay'. If null, the system is detected from the file name.
#' @return A tibble
#' @export
#'
nrp_read_zooplankton_file <- function(path, db_path = getOption("nrp.db_path",
                                                                file.choose()), system = NULL) {
  check_file_exists(path)
  chk::chk_null_or(system, chk = chk::chk_chr)
  if(is.null(system)){
    if(str_detect(tolower(basename(path)), "^ar")){
      system <- "AR"
    } else if(str_detect(tolower(basename(path)), "^kl")) {
      system <- "KL"
    } else {
      err("System cannot be detected from file name. Please set system argument to 'arrow' or 'kootenay'.")
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

  chk::check_key(data, key = c("Date", "Station", "Replicate", "FileName"))

  data %<>%
    mutate(Station = as.integer(str_extract(.data$Station, "\\d"))) %>%
    clean_input_cols(lookup = nrp::zoo_input_cols) %>%
    mutate(Station = paste0(system, .data$Station))

  sites <- nrp_download_sites(db_path = db_path)

  if(!all(unique(data$Station) %in% sites$SiteID)) err("Unknown Stations in raw data.")

  data
}

#' Read zooplankton raw data files
#'
#' @param path A string of the path to the directory.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @param system The system 'arrow' or 'kootenay'. If null, the system is detected from the file names.
#' @inheritParams fs::dir_ls
#' @return A tibble.
#' @export
#'
nrp_read_zooplankton <- function(path = ".", db_path = getOption("nrp.db_path", file.choose()),
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

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_zooplankton_file(., db_path = db_path, system = system))))
  rownames(datas) <- NULL

  datas
}

#' Upload zooplankton data to the nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An SQLite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
nrp_upload_zooplankton<- function(data, db_path = getOption("nrp.db_path", file.choose()),
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

  check_zoo_raw_data(data, exclusive = TRUE, order = TRUE)

  zoo_sample <- select(data, c(.data$Date, SiteID = .data$Station, .data$Replicate,
                               .data$FileName, .data$MonthCat, EndRev = .data$ENDREV,
                               StartRev = .data$STARTREV, SplMade = .data$SPLmade,
                               SplCount = .data$SPLcount, .data$FundingSource, .data$FieldCollection,
                               .data$Analyst))

  readwritesqlite::rws_write(x = zoo_sample, commit = commit,
                             strict = strict, silent = silent,
                             x_name = "ZooplanktonSample", conn = conn, replace = replace)

  zoo_data <- select(data, c(.data$Date, SiteID = .data$Station, .data$Replicate, .data$FileName,
                             .data$DenTotal, .data$DCopep, .data$DClad, .data$`DClad other than Daph`,
                             .data$DDash, .data$DDkenai, .data$DEpi, .data$DCycl, .data$DNaup,
                             .data$DDaph, .data$DDiaph, .data$DBosm, .data$DScap, .data$DLepto,
                             .data$DCerio, .data$DChyd, .data$DOtherCopep, .data$DOtherClad,
                             .data$DDashM, .data$DDashF, .data$DDash5, .data$DDash4, .data$DDash3,
                             .data$DDash2, .data$DDash1, .data$DDashC, .data$DDkenaiM, .data$DDkenaiF,
                             .data$DDkenaiC, .data$DEpiM, .data$DEpiF, .data$DEpiC, .data$DCyclM,
                             .data$DCyclF, .data$DCycl5, .data$DCycl4, .data$DCycl3, .data$DCycl2,
                             .data$DCycl1, .data$DCyclC, .data$BiomTotal, .data$BCopep, .data$BClad,
                             .data$`BClad other than Daph`, .data$BDash, .data$BDkenai, .data$BEpi,
                             .data$BCycl, .data$BNaup, .data$BDaph, .data$BDiaph, .data$BBosm,
                             .data$BScap, .data$BLepto, .data$BCerio, .data$BChyd, .data$BOtherCopep,
                             .data$BOtherClad, .data$BDashM, .data$BDashF, .data$BDash5, .data$BDash4,
                             .data$BDash3, .data$BDash2, .data$BDash1, .data$BDashC, .data$BDkenaiM,
                             .data$BDkenaiF, .data$BDkenaiC, .data$BEpiM, .data$BEpiF, .data$BEpiC,
                             .data$BCyclM, .data$BCyclF, .data$BCycl5, .data$BCycl4, .data$BCycl3,
                             .data$BCycl2, .data$BCycl1, .data$BCyclC, .data$F1Dash, .data$F1Dkenai,
                             .data$F1Epi, .data$F1Cycl, .data$F1Daph, .data$F1Diaph, .data$F1Bosm,
                             .data$F1Scap, .data$F1Lepto, .data$F1Cerio, .data$F1Chyd, .data$F2Dash,
                             .data$F2Dkenai, .data$F2Epi, .data$F2Cycl, .data$F2Daph, .data$F2Diaph,
                             .data$F2Bosm, .data$F2Scap, .data$F2Lepto, .data$F2Cerio, .data$F2Chyd,
                             .data$F3Dash, .data$F3Dkenai, .data$F3Epi, .data$F3Cycl, .data$F3Daph,
                             .data$F3Diaph, .data$F3Bosm, .data$F3Scap, .data$F3Lepto, .data$F3Cerio,
                             .data$F3Chyd, .data$F4Dash, .data$F4Dkenai, .data$F4Epi, .data$F4Cycl,
                             .data$F4Daph, .data$F4Diaph, .data$F4Bosm, .data$F4Scap, .data$F4Lepto,
                             .data$F4Cerio, .data$F4Chyd, .data$F5Dash, .data$F5Dkenai, .data$F5Epi,
                             .data$F5Cycl, .data$F5Daph, .data$F5Diaph, .data$F5Bosm, .data$F5Scap,
                             .data$F5Lepto, .data$F5Cerio, .data$F5Chyd, .data$F6Dash, .data$F6Dkenai,
                             .data$F6Epi, .data$F6Cycl, .data$F6Daph, .data$F6Diaph, .data$F6Bosm,
                             .data$F6Scap, .data$F6Lepto, .data$F6Cerio, .data$F6Chyd)) %>%
    tidyr::pivot_longer(cols = -c(.data$Date, .data$SiteID, .data$Replicate, .data$FileName),
                        names_to = "Parameter", values_to = "Value") %>%
    mutate(RawCount = NA_integer_)

  readwritesqlite::rws_write(x = zoo_data, commit = commit, strict = strict,
                             silent = silent,
                             x_name = "Zooplankton", conn = conn, replace = replace)
}

#' Download Zooplankton sample table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return Zooplankton sample table
#' @export
#'
nrp_download_zoo_sample <- function(db_path = getOption("nrp.db_path", file.choose())) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }
  readwritesqlite::rws_read_table("ZooplanktonSample", conn = conn)
}

#' Download Zooplankton data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param parameters A character vector of the parameters to include.
#' Permissable values can be found in the nrp::zoo_params
#' @param counts A flag indicating whether to return the raw counts instead.
#' @param db_path The SQLite connection object or path to the SQLite database
#'
#' @return CTD data table
#' @export
#'
nrp_download_zooplankton <- function(start_date = NULL, end_date = NULL,
                                     sites = NULL, parameters = "all", counts = FALSE,
                                     db_path = getOption("nrp.db_path", file.choose())){

  chk::chk_null_or(sites, chk = chk::chk_character)
  chk::chk_character(parameters)
  chk::chk_flag(counts)
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
    parameters <- nrp::zoo_params
  } else if(!all(parameters %in% nrp::zoo_params)){
    err(paste("1 or more invalid parameter names"))
  }

  dates <- fill_date_query(table = "Zooplankton", col = "Date", end = end_date, start = start_date,
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

  cols <- c("Date", "SiteID", "Replicate", "FileName", "Parameter", "Value")
  if(counts) cols[cols == "Value"] <- "RawCount"
  colsSql <- cc(cols, ellipsis = 1000, brac = "`")

  query <- paste0("SELECT", colsSql, "FROM Zooplankton WHERE ((`Date` >= ", start_dateSql, ") AND (`Date` <= ",
                  end_dateSql, ") AND (`SiteID` IN (", sitesSql,")) AND (`Parameter` IN (", paramsSql,")))")

  result <- readwritesqlite::rws_query(query = query, conn = conn, meta = TRUE) %>%
    dplyr::mutate(Date = dttr2::dtt_date(.data$Date))

  if(nrow(result) == 0) warning("no data available for query provided.")

  if(counts){
    result %<>% tidyr::pivot_wider(names_from = "Parameter", values_from = "RawCount")
  } else {
    result %<>% tidyr::pivot_wider(names_from = "Parameter", values_from = "Value")
  }

  result
}
