#' Extract and clean EMS data
#'
#' @param data The EMS data to be extracted. Data must first be downloaded from server by using rems package
#' @param db_path The SQLite connection object or path to the SQLite database
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @return A a data frame
#' @export

nrp_extract_ems <- function(data, db_path, analysis_type = "standard"){

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  sites <- nrp::emsSites
  sf::st_geometry(sites) <- NULL

  params <- nrp::ems_param_lookup

  data %<>% filter(.data$EMS_ID %in% sites$EmsSite) %>%
    mutate(COLLECTION_START = lubridate::ymd_hms(.data$COLLECTION_START, tz = "Etc/GMT+8"),
           COLLECTION_END = lubridate::ymd_hms(.data$COLLECTION_END, tz = "Etc/GMT+8")) %>%
    select(.data$EMS_ID, .data$MONITORING_LOCATION, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
           .data$PARAMETER, .data$RESULT, .data$ANALYZING_AGENCY, .data$RESULT_LETTER, .data$UPPER_DEPTH,
           .data$LOWER_DEPTH, .data$ANALYTICAL_METHOD, .data$UNIT) %>%
    mutate(PARAMETER = ifelse(.data$PARAMETER %in% c("Phosphorus Total Dissolved") &
                                .data$ANALYTICAL_METHOD == "ICP",
                              "Phosphorus Total Dissolved metals", .data$PARAMETER)) %>%
    mutate(PARAMETER = ifelse(.data$PARAMETER == "Phosphorus Total" &
                                .data$ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
                              "Phosphorus Total metals", .data$PARAMETER)) %>%
    mutate(RESULT_LETTER = gsub("M", NA, .data$RESULT_LETTER),
           RESULT = paste0(.data$RESULT, "/", .data$RESULT_LETTER),
           RESULT = gsub(c("NA"), "", .data$RESULT)) %>%
    select(-.data$ANALYTICAL_METHOD, -.data$RESULT_LETTER, -.data$EMS_ID) %>%
    full_join(select(sites, .data$SiteID, .data$SiteName), by = c("MONITORING_LOCATION" = "SiteName")) %>%
    select(-.data$MONITORING_LOCATION)

  if(analysis_type == "standard"){
    params_standard <- params$PARAMETER[params$Comment == "standard analysis"]

    data %<>% filter(.data$PARAMETER %in% params_standard) %>%
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT),
             REQUISITION_ID = as.numeric(.data$REQUISITION_ID)) %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START)

    key_cols <- c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
                  "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH")

    data %<>% clean_key_cols(key_cols) %>%
      group_by(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
               .data$ANALYZING_AGENCY, .data$UPPER_DEPTH, .data$LOWER_DEPTH) %>%
      mutate(ReplicateID = 1:n()) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_standard)
    standard_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(standard_units, fill_units)

  } else if(analysis_type == "metals"){
    params_metals <- params$PARAMETER[params$Comment == "metals"]

    data %<>% filter(.data$PARAMETER %in% params_metals) %>%
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT),
             REQUISITION_ID = as.numeric(.data$REQUISITION_ID)) %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START)

    key_cols <- c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
                  "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH")

    data %<>% clean_key_cols(key_cols) %>%
      group_by(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
               .data$ANALYZING_AGENCY, .data$UPPER_DEPTH, .data$LOWER_DEPTH) %>%
      mutate(ReplicateID = 1:n()) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_metals)
    metals_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(metals_units, fill_units)

  } else {
    err("analysis_type must be either 'standard' or 'metals'")
  }

  data %<>% mutate(LOWER_DEPTH = units::set_units(.data$LOWER_DEPTH, "m"),
                   UPPER_DEPTH = units::set_units(.data$UPPER_DEPTH, "m")) %>%
    select(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
                   .data$ANALYZING_AGENCY, .data$UPPER_DEPTH, .data$LOWER_DEPTH, .data$ReplicateID,
                   everything(), -.data$row_id)
  data
}


add_ems_detection_limit_cols <- function(data, params, sep = "/"){
  for(name in names(data)){
    base_name <- gsub('unit:.*', "", name)
    if(grepl("limit", name)){
      next()
    }
    else if(base_name %in% params){
      data %<>% tidyr::separate(name, c(paste("value", name), paste("Limit", name)), sep = sep) %>%
        mutate_at(vars(paste("Limit", name)), funs(if_else(.data$. == "<", .data$., NA_character_))) %>%
        mutate_at(vars(paste("Limit", name)),
                  funs(if_else(.data$. == "<", eval(parse(text = paste0("`","value ", name, "`"))), .data$.))) %>%
        mutate_at(vars(paste("value", name)),
                  funs(if_else(!is.na(eval(parse(text = paste0("`","Limit ", name, "`")))), 0, as.numeric(.data$.)))) %>%
        mutate_at(vars(paste("Limit", name)), funs(as.numeric))
    }
  }
  names(data) <- gsub("value ", "", names(data))
  data
}

#' Upload standard EMS data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
# conn <- nrp_create_db(path = ":memory:", ask = FALSE)
# path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
# ems <- readRDS(path)
# data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")

nrp_upload_ems_standard<- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_standard_data(data, exclusive = TRUE, order = TRUE)

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                             x_name = "standardEMS", conn = conn)
}

#' Upload EMS metals data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
nrp_upload_ems_metals <- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_metals_data(data, exclusive = TRUE, order = TRUE)

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                             x_name = "metalsEMS", conn = conn)
}

#' Dowload EMS data table from database
#'
#' @param db_path The SQLite connection object or path to the SQLite database
#' @param start_date_time The start date
#' @param end_date_time The end date
#' @param sites A character vector of the Site IDs
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @param show_detection_limits Whether to include detection limit columns for each parameter
#' @return EMS data table
#' @export
#'
nrp_download_ems <- function(db_path = getOption("nrp.db_path", NULL), start_date_time = "2018-01-01 00:00:00",
                             end_date_time = "2018-12-31 00:00:00", sites = NULL, analysis_type = "standard",
                             show_detection_limits = FALSE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  if(start_date_time > end_date_time){
    err("start date is later than end date")
  }
  if(end_date_time > Sys.Date()){
    err("end date is later than present day")
  }

  checkr::check_datetime(as.POSIXct(start_date_time))
  checkr::check_datetime(as.POSIXct(end_date_time))

  site_table <- nrp::emsSites

  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }

  Date <- NULL
  SiteID <- NULL

  sitesSql <- cc(sites, ellipsis = length(sites) + 1)
  start_dateSql <- paste0("'", start_date_time, "'")
  end_dateSql <- paste0("'", end_date_time, "'")

  if(analysis_type == "standard"){
    table <- "standardEMS"
  } else if(analysis_type == "metals"){
    table <- "metalsEMS"
  }

  query <- paste0("SELECT * FROM ", table, " WHERE ((`COLLECTION_START` >= ", start_dateSql, ") AND (`COLLECTION_START` <= ",
                  end_dateSql, ") AND (`SiteID` IN (", sitesSql,")))")

  result <- readwritesqlite::rws_query(query = query, conn = conn) %>%
    dplyr::mutate(COLLECTION_START = dttr::dtt_date_time(.data$COLLECTION_START),
                  COLLECTION_END = dttr::dtt_date_time(.data$COLLECTION_END))

  if(show_detection_limits == FALSE){
    result %<>% select(-c(grep("Limit", names(result))))
  }

  result
}

clean_key_cols <- function(data, cols) {
  cleaned_cols <- stats::complete.cases(data[, cols])
  return(data[cleaned_cols, ])
}
