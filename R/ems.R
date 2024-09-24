#' Extract and clean EMS data
#'
#' @param data The EMS data to be extracted. Data must first be downloaded from server by using rems package
#' @param db_path The SQLite connection object or path to the nrp SQLite database
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @return A tibble.
#' @export

nrp_extract_ems <- function(data, db_path = getOption("nrp.db_path", file.choose()), analysis_type = "standard"){

  chk::chk_chr(analysis_type)
  if(!analysis_type %in% c("standard", "metals")){
    err("analysis_type must be either 'standard' or 'metals'")
  }

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_raw_data(data, exclusive = TRUE, order = TRUE)

  sites <- nrp_download_sites(db_path = conn) %>%
    filter(!is.na(.data$EmsSiteNumber)) %>%
    filter(!is.na(.data$EmsSiteName))
  sf::st_geometry(sites) <- NULL

  params <- nrp::ems_param_lookup

  data %<>% filter(.data$EMS_ID %in% sites$EmsSiteNumber) %>%
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
    left_join(select(sites, .data$SiteID, .data$EmsSiteName), by = c("MONITORING_LOCATION" = "EmsSiteName")) %>%
    select(-.data$MONITORING_LOCATION)

  if(analysis_type == "standard"){
    params_standard <- params$PARAMETER[params$Comment == "standard analysis"]

    data %<>% filter(.data$PARAMETER %in% params_standard) %>%
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT),
             REQUISITION_ID = as.numeric(.data$REQUISITION_ID)) %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = seq_len(n())) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START)


    key_cols <- c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
                  "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH")

    data %<>% clean_key_cols(key_cols)

    data %<>% group_by(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
               .data$ANALYZING_AGENCY, .data$UPPER_DEPTH, .data$LOWER_DEPTH) %>%
      mutate(ReplicateID = seq_len(n())) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_standard)
    standard_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(standard_units, fill_units) %>%
      select(.data$`SiteID`, .data$`COLLECTION_START`, .data$`COLLECTION_END`, .data$`REQUISITION_ID`, .data$`ANALYZING_AGENCY`,
             .data$`UPPER_DEPTH`, .data$`LOWER_DEPTH`, .data$`ReplicateID`, .data$`Alkalinity Total 4.5`,
             .data$`Limit Alkalinity Total 4.5`, .data$`Carbon Total Inorganic`, .data$`Limit Carbon Total Inorganic`,
             .data$`Carbon Total Organic`, .data$`Limit Carbon Total Organic`, .data$`Carbon Total`,
             .data$`Limit Carbon Total`,.data$`Chlorophyll A`, .data$`Limit Chlorophyll A`, .data$`Nitrate (NO3) Dissolved`,
             .data$`Limit Nitrate (NO3) Dissolved`, .data$`Nitrate(NO3) + Nitrite(NO2) Dissolved`,
             .data$`Limit Nitrate(NO3) + Nitrite(NO2) Dissolved`, .data$`Nitrogen - Nitrite Dissolved (NO2)`,
             .data$`Limit Nitrogen - Nitrite Dissolved (NO2)`, .data$`Nitrogen Ammonia Total`,
             .data$`Limit Nitrogen Ammonia Total`, .data$`Nitrogen Total`,
             .data$`Limit Nitrogen Total`, .data$`Phosphorus Ort.Dis-P`, .data$`Limit Phosphorus Ort.Dis-P`,
             .data$`Phosphorus Total Dissolved`, .data$`Limit Phosphorus Total Dissolved`, .data$`Phosphorus Total`,
             .data$`Limit Phosphorus Total`, .data$`pH`, .data$`Limit pH`, .data$`Silica Reactive Diss`,
             .data$`Limit Silica Reactive Diss`, .data$`Turbidity`, .data$`Limit Turbidity`, .data$row_id)


  } else if(analysis_type == "metals"){
    params_metals <- params$PARAMETER[params$Comment == "metals"]

    data %<>% filter(.data$PARAMETER %in% params_metals) %>%
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT),
             REQUISITION_ID = as.numeric(.data$REQUISITION_ID)) %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = seq_len(n())) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START)

    if(!"Hydroxide Alkalinityunit:mg/L" %in% names(data)){
      data$`Hydroxide Alkalinityunit:mg/L` <- NA_real_
    }

    key_cols <- c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
                  "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH")

    data %<>% clean_key_cols(key_cols) %>%
      group_by(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID,
               .data$ANALYZING_AGENCY, .data$UPPER_DEPTH, .data$LOWER_DEPTH) %>%
      mutate(ReplicateID = seq_len(n())) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_metals)
    metals_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(metals_units, fill_units)
    data %<>% select(.data$SiteID, .data$COLLECTION_START, .data$COLLECTION_END, .data$REQUISITION_ID, .data$ANALYZING_AGENCY,
             .data$UPPER_DEPTH, .data$LOWER_DEPTH, .data$ReplicateID, .data$`Alkalinity Phen. 8.3`,
             .data$`Limit Alkalinity Phen. 8.3`, .data$`Aluminum Dissolved`, .data$`Limit Aluminum Dissolved`,
             .data$`Aluminum Total`, .data$`Limit Aluminum Total`, .data$`Antimony Dissolved`,
             .data$`Limit Antimony Dissolved`, .data$`Antimony Total`, .data$`Limit Antimony Total`,
             .data$`Arsenic Dissolved`, .data$`Limit Arsenic Dissolved`, .data$`Arsenic Total`, .data$`Limit Arsenic Total`,
             .data$`Barium Dissolved`, .data$`Limit Barium Dissolved`, .data$`Barium Total`, .data$`Limit Barium Total`,
             .data$`Beryllium Dissolved`, .data$`Limit Beryllium Dissolved`, .data$`Beryllium Total`,
             .data$`Limit Beryllium Total`, .data$`Bicarbonate Alkalinity`, .data$`Limit Bicarbonate Alkalinity`,
             .data$`Bismuth Dissolved`, .data$`Limit Bismuth Dissolved`, .data$`Bismuth Total`, .data$`Limit Bismuth Total`,
             .data$`Boron Dissolved`, .data$`Limit Boron Dissolved`, .data$`Boron Total`, .data$`Limit Boron Total`,
             .data$`Cadmium Dissolved`, .data$`Limit Cadmium Dissolved`, .data$`Cadmium Total`, .data$`Limit Cadmium Total`,
             .data$`Calcium Dissolved`, .data$`Limit Calcium Dissolved`, .data$`Calcium Total`, .data$`Limit Calcium Total`,
             .data$`Carbonate Alkalinity`, .data$`Limit Carbonate Alkalinity`, .data$`Chromium Dissolved`,
             .data$`Limit Chromium Dissolved`, .data$`Chromium Total`, .data$`Limit Chromium Total`,
             .data$`Cobalt Dissolved`, .data$`Limit Cobalt Dissolved`, .data$`Cobalt Total`, .data$`Limit Cobalt Total`,
             .data$`Copper Dissolved`, .data$`Limit Copper Dissolved`, .data$`Copper Total`, .data$`Limit Copper Total`,
             .data$`Hardness (Dissolved)`, .data$`Limit Hardness (Dissolved)`, .data$`Hardness Total (Total)`,
             .data$`Limit Hardness Total (Total)`, .data$`Hydroxide Alkalinity`, .data$`Limit Hydroxide Alkalinity`,
             .data$`Iron Dissolved`, .data$`Limit Iron Dissolved`, .data$`Iron Total`, .data$`Limit Iron Total`,
             .data$`Lead Dissolved`, .data$`Limit Lead Dissolved`,.data$`Lead Total`, .data$`Limit Lead Total`,
             .data$`Magnesium Dissolved`, .data$`Limit Magnesium Dissolved`, .data$`Magnesium Total`,
             .data$`Limit Magnesium Total`, .data$`Manganese Dissolved`, .data$`Limit Manganese Dissolved`,
             .data$`Manganese Total`, .data$`Limit Manganese Total`, .data$`Molybdenum Dissolved`,
             .data$`Limit Molybdenum Dissolved`, .data$`Molybdenum Total`, .data$`Limit Molybdenum Total`,
             .data$`Nickel Dissolved`, .data$`Limit Nickel Dissolved`, .data$`Nickel Total`, .data$`Limit Nickel Total`,
             .data$`Phosphorus Total Dissolved metals`, .data$`Limit Phosphorus Total Dissolved metals`,
             .data$`Phosphorus Total metals`, .data$`Limit Phosphorus Total metals`, .data$`Potassium Dissolved`,
             .data$`Limit Potassium Dissolved`, .data$`Potassium Total`, .data$`Limit Potassium Total`,
             .data$`Selenium Dissolved`, .data$`Limit Selenium Dissolved`, .data$`Selenium Total`,
             .data$`Limit Selenium Total`, .data$`Silicon Dissolved`, .data$`Limit Silicon Dissolved`,
             .data$`Silicon Total`, .data$`Limit Silicon Total`, .data$`Silver Dissolved`, .data$`Limit Silver Dissolved`,
             .data$`Silver Total`, .data$`Limit Silver Total`, .data$`Sodium Dissolved`, .data$`Limit Sodium Dissolved`,
             .data$`Sodium Total`, .data$`Limit Sodium Total`, .data$`Strontium Dissolved`, .data$`Limit Strontium Dissolved`,
             .data$`Strontium Total`, .data$`Limit Strontium Total`, .data$`Sulfur Dissolved`,
             .data$`Limit Sulfur Dissolved`, .data$`Sulfur Total`, .data$`Limit Sulfur Total`, .data$`Thallium Dissolved`,
             .data$`Limit Thallium Dissolved`, .data$`Thallium Total`, .data$`Limit Thallium Total`, .data$`Tin Dissolved`,
             .data$`Limit Tin Dissolved`, .data$`Tin Total`, .data$`Limit Tin Total`, .data$`Titanium Dissolved`,
             .data$`Limit Titanium Dissolved`, .data$`Titanium Total`, .data$`Limit Titanium Total`,
             .data$`Uranium Dissolved`, .data$`Limit Uranium Dissolved`, .data$`Uranium Total`, .data$`Limit Uranium Total`,
             .data$`Vanadium Dissolved`, .data$`Limit Vanadium Dissolved`, .data$`Vanadium Total`,
             .data$`Limit Vanadium Total`, .data$`Zinc Dissolved`, .data$`Limit Zinc Dissolved`, .data$`Zinc Total`,
             .data$`Limit Zinc Total`, .data$row_id)
  }

  data %<>% mutate(LOWER_DEPTH = units::set_units(as.numeric(.data$LOWER_DEPTH), "m"),
                   UPPER_DEPTH = units::set_units(as.numeric(.data$UPPER_DEPTH), "m")) %>%
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
        mutate_at(vars(paste("Limit", name)), list(~if_else(eval(parse(text = paste0("`","Limit ", name, "`"))) == "<",
                                                            eval(parse(text = paste0("`","Limit ", name, "`"))),
                                                            NA_character_))) %>%
        mutate_at(vars(paste("Limit", name)),
                  list(~if_else(eval(parse(text = paste0("`","Limit ", name, "`"))) == "<",
                                eval(parse(text = paste0("`","value ", name, "`"))),
                                eval(parse(text = paste0("`","Limit ", name, "`")))))) %>%
        mutate_at(vars(paste("value", name)),
                  list(~if_else(!is.na(eval(parse(text = paste0("`","Limit ", name, "`")))),
                                0, as.numeric(eval(parse(text = paste0("`","value ", name, "`"))))))) %>%
        mutate_at(vars(paste("Limit", name)), list(~as.numeric(eval(parse(text = paste0("`","Limit ", name, "`"))))))
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
nrp_upload_ems_standard<- function(data, db_path = getOption("nrp.db_path", file.choose()), commit = TRUE,
                                   strict = TRUE, silent = TRUE, replace = FALSE){
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_standard_data(data, exclusive = TRUE, order = TRUE)

  if(replace == FALSE){

    query <- "SELECT COLLECTION_START FROM standardEMS WHERE COLLECTION_START = (SELECT MAX(COLLECTION_START) FROM standardEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)

    if(!nrow(result) == 0){
      last_date <- result[[1, 1]]
      date_4yr <- dttr2::dtt_date_time(as.numeric(last_date) - 126316800, tz = "Etc/GMT+8")
      query <- paste0("SELECT * FROM standardEMS WHERE ((`COLLECTION_START` >= '", date_4yr, "') AND (`COLLECTION_START` <= '",
                      last_date, "'))")
      ems_4year <- readwritesqlite::rws_query(query = query, conn = conn)

      data %<>% setdiff(ems_4year) %>%
        filter(.data$COLLECTION_START > last_date)

        if(nrow(data) == 0){
          err("The data you are attempting to upload is already in the database")
        } else {
          message(paste("Trimmed new data.", nrow(data), "new rows to be included in upload for date range",
                        first(data$COLLECTION_START), "-", last(data$COLLECTION_START)))
      }
    }
  }

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                             replace = replace, x_name = "standardEMS", conn = conn)
}

# rws_read_table("StandardEMS" , conn = conn)

#' Upload EMS metals data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
nrp_upload_ems_metals <- function(data, db_path = getOption("nrp.db_path", file.choose()), commit = TRUE,
                                  strict = TRUE, silent = TRUE, replace = FALSE){
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_metals_data(data, exclusive = TRUE, order = TRUE)

  if(replace == FALSE){

    query <- "SELECT COLLECTION_START FROM metalsEMS WHERE COLLECTION_START = (SELECT MAX(COLLECTION_START) FROM metalsEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)

    if(!nrow(result) == 0){
      last_date <- result[[1, 1]]
      date_4yr <- dttr2::dtt_date_time(as.numeric(last_date) - 126316800, tz = "Etc/GMT+8")
      query <- paste0("SELECT * FROM metalsEMS WHERE ((`COLLECTION_START` >= '", date_4yr, "') AND (`COLLECTION_START` <= '",
                      last_date, "'))")
      ems_4year <- readwritesqlite::rws_query(query = query, conn = conn)

      data %<>% setdiff(ems_4year) %>%
        filter(.data$COLLECTION_START > last_date)

      if(nrow(data) == 0){
        err("The data you are attempting to upload is already in the database")
      } else {
        message(paste("Trimmed new data.", nrow(data), "new rows to be included in upload for date range",
                      first(data$COLLECTION_START), "-", last(data$COLLECTION_START)))
      }
    }
  }
  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                             replace = replace, x_name = "metalsEMS", conn = conn)
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
nrp_download_ems <- function(db_path = getOption("nrp.db_path", file.choose()), start_date_time = NULL,
                             end_date_time = NULL, sites = NULL, analysis_type = "standard",
                             show_detection_limits = FALSE){
  chk::chk_null_or(start_date_time, chk = check_chr_date)
  chk::chk_null_or(end_date_time, chk = check_chr_date)
  chk::chk_chr(analysis_type)
  chk::chk_null_or(sites, chk = chk::chk_character)
  chk::chk_flag(show_detection_limits)

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  site_table <- nrp::emsSites

  if(is.null(sites)){
    sites <- site_table$SiteID
  }
  if(!all(sites %in% site_table$SiteID)){
    err(paste("1 or more invalid site names"))
  }

  Date <- NULL
  SiteID <- NULL

  if(analysis_type == "standard"){
    table <- "standardEMS"
  } else if(analysis_type == "metals"){
    table <- "metalsEMS"
  }

  dates <- fill_date_time_query(table = table, col = "COLLECTION_START", end = end_date_time, start = start_date_time,
                                connection = conn)
  start_date <- dates["start_date_time"][[1]]
  end_date <- dates["end_date_time"][[1]]

  if(start_date > end_date){
    err("start date is later than end date")
  }

  sitesSql <- cc(sites, ellipsis = 1000)
  start_dateSql <- paste0("'", start_date_time, "'")
  end_dateSql <- paste0("'", end_date_time, "'")

  query <- paste0("SELECT * FROM ", table, " WHERE ((`COLLECTION_START` >= ", start_dateSql, ") AND (`COLLECTION_START` <= ",
                  end_dateSql, ") AND (`SiteID` IN (", sitesSql,")))")

  result <- readwritesqlite::rws_query(query = query, conn = conn) %>%
    dplyr::mutate(COLLECTION_START = dttr2::dtt_date_time(.data$COLLECTION_START),
                  COLLECTION_END = dttr2::dtt_date_time(.data$COLLECTION_END))

  if(show_detection_limits == FALSE){
    result %<>% select(-c(grep("Limit", names(result))))
  }

  result
}

clean_key_cols <- function(data, cols) {
  cleaned_cols <- stats::complete.cases(data[, cols])
  data[cleaned_cols, ]
}
