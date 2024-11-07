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
    mutate(Station = as.integer(str_extract(Station, "\\d"))) %>%
    clean_input_cols(lookup = nrp::zoo_input_cols) %>%
    mutate(Station = paste0(system, Station))

  sites <- nrp_download_sites(db_path = db_path)

  if(!all(unique(data$Station) %in% sites$SiteID)) warning("Sites present in input data that are not found in 'Sites' table in database.")

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

  zoo_sample <- select(data, c(Date, SiteID = Station, Replicate,
                               FileName, MonthCat, EndRev = ENDREV,
                               StartRev = STARTREV, SplMade = SPLmade,
                               SplCount = SPLcount, FundingSource, FieldCollection,
                               Analyst))

  readwritesqlite::rws_write(x = zoo_sample, commit = commit,
                             strict = strict, silent = silent,
                             x_name = "ZooplanktonSample", conn = conn, replace = replace)

  zoo_data <- select(data, c(Date, SiteID = Station, Replicate, FileName,
                             DenTotal, DCopep, DClad, `DClad other than Daph`,
                             DDash, DDkenai, DEpi, DCycl, DNaup,
                             DDaph, DDiaph, DBosm, DScap, DLepto,
                             DCerio, DChyd, DOtherCopep, DOtherClad,
                             DDashM, DDashF, DDash5, DDash4, DDash3,
                             DDash2, DDash1, DDashC, DDkenaiM, DDkenaiF,
                             DDkenaiC, DEpiM, DEpiF, DEpiC, DCyclM,
                             DCyclF, DCycl5, DCycl4, DCycl3, DCycl2,
                             DCycl1, DCyclC, BiomTotal, BCopep, BClad,
                             `BClad other than Daph`, BDash, BDkenai, BEpi,
                             BCycl, BNaup, BDaph, BDiaph, BBosm,
                             BScap, BLepto, BCerio, BChyd, BOtherCopep,
                             BOtherClad, BDashM, BDashF, BDash5, BDash4,
                             BDash3, BDash2, BDash1, BDashC, BDkenaiM,
                             BDkenaiF, BDkenaiC, BEpiM, BEpiF, BEpiC,
                             BCyclM, BCyclF, BCycl5, BCycl4, BCycl3,
                             BCycl2, BCycl1, BCyclC, F1Dash, F1Dkenai,
                             F1Epi, F1Cycl, F1Daph, F1Diaph, F1Bosm,
                             F1Scap, F1Lepto, F1Cerio, F1Chyd, F2Dash,
                             F2Dkenai, F2Epi, F2Cycl, F2Daph, F2Diaph,
                             F2Bosm, F2Scap, F2Lepto, F2Cerio, F2Chyd,
                             F3Dash, F3Dkenai, F3Epi, F3Cycl, F3Daph,
                             F3Diaph, F3Bosm, F3Scap, F3Lepto, F3Cerio,
                             F3Chyd, F4Dash, F4Dkenai, F4Epi, F4Cycl,
                             F4Daph, F4Diaph, F4Bosm, F4Scap, F4Lepto,
                             F4Cerio, F4Chyd, F5Dash, F5Dkenai, F5Epi,
                             F5Cycl, F5Daph, F5Diaph, F5Bosm, F5Scap,
                             F5Lepto, F5Cerio, F5Chyd, F6Dash, F6Dkenai,
                             F6Epi, F6Cycl, F6Daph, F6Diaph, F6Bosm,
                             F6Scap, F6Lepto, F6Cerio, F6Chyd)) %>%
    tidyr::pivot_longer(cols = -c(Date, SiteID, Replicate, FileName),
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
    dplyr::mutate(Date = dttr2::dtt_date(Date))

  if(nrow(result) == 0) warning("no data available for query provided.")

  if(counts){
    result %<>% tidyr::pivot_wider(names_from = "Parameter", values_from = "RawCount")
  } else {
    result %<>% tidyr::pivot_wider(names_from = "Parameter", values_from = "Value")
  }

  result
}
