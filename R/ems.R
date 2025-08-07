#' Extract and clean EMS data
#'
#' @param data The EMS data to be extracted. Data must first be downloaded from server by using rems package
#' @param db_path The SQLite connection object or path to the nrp SQLite database
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @return A tibble.
#' @export

nrp_extract_ems <- function(data, db_path = getOption("nrp.db_path", file.choose()), analysis_type = "standard") {
  chk::chk_chr(analysis_type)
  if (!analysis_type %in% c("standard", "metals")) {
    err("analysis_type must be either 'standard' or 'metals'")
  }

  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_raw_data(data, exclusive = TRUE, order = TRUE)

  sites <- nrp_download_sites(db_path = conn) %>%
    filter(!is.na(EmsSiteNumber)) %>%
    filter(!is.na(EmsSiteName))
  sf::st_geometry(sites) <- NULL

  params <- nrp::ems_param_lookup

  data %<>% filter(EMS_ID %in% sites$EmsSiteNumber) %>%
    mutate(
      COLLECTION_START = lubridate::ymd_hms(COLLECTION_START, tz = "Etc/GMT+8"),
      COLLECTION_END = lubridate::ymd_hms(COLLECTION_END, tz = "Etc/GMT+8")
    ) %>%
    select(
      EMS_ID, MONITORING_LOCATION, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
      PARAMETER, RESULT, ANALYZING_AGENCY, RESULT_LETTER, UPPER_DEPTH,
      LOWER_DEPTH, ANALYTICAL_METHOD, UNIT
    ) %>%
    mutate(PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total Dissolved") &
      ANALYTICAL_METHOD == "ICP",
    "Phosphorus Total Dissolved metals", PARAMETER
    )) %>%
    mutate(PARAMETER = ifelse(PARAMETER == "Phosphorus Total" &
      ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
    "Phosphorus Total metals", PARAMETER
    )) %>%
    mutate(
      RESULT_LETTER = gsub("M", NA, RESULT_LETTER),
      RESULT = paste0(RESULT, "/", RESULT_LETTER),
      RESULT = gsub(c("NA"), "", RESULT)
    ) %>%
    select(-ANALYTICAL_METHOD, -RESULT_LETTER, -EMS_ID) %>%
    left_join(select(sites, SiteID, EmsSiteName), by = c("MONITORING_LOCATION" = "EmsSiteName")) %>%
    select(-MONITORING_LOCATION)

  if (analysis_type == "standard") {
    params_standard <- params$PARAMETER[params$Comment == "standard analysis"]

    data %<>% filter(PARAMETER %in% params_standard) %>%
      mutate(
        PARAMETER = paste0(PARAMETER, "unit:", UNIT),
        REQUISITION_ID = as.numeric(REQUISITION_ID)
      ) %>%
      select(-UNIT) %>%
      group_by_at(vars(-RESULT)) %>%
      mutate(row_id = seq_len(n())) %>%
      ungroup() %>%
      tidyr::spread(key = PARAMETER, value = RESULT, fill = NA) %>%
      arrange(COLLECTION_START)


    key_cols <- c(
      "SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
      "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH"
    )

    data %<>% clean_key_cols(key_cols)

    data %<>% group_by(
      SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
      ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH
    ) %>%
      mutate(ReplicateID = seq_len(n())) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_standard)
    standard_units <- pull_ems_units(data)
    names(data) <- gsub("unit:.*", "", names(data))
    data %<>% map2_dfc(standard_units, fill_units) %>%
      select(
        `SiteID`, `COLLECTION_START`, `COLLECTION_END`, `REQUISITION_ID`, `ANALYZING_AGENCY`,
        `UPPER_DEPTH`, `LOWER_DEPTH`, `ReplicateID`, `Alkalinity Total 4.5`,
        `Limit Alkalinity Total 4.5`, `Carbon Total Inorganic`, `Limit Carbon Total Inorganic`,
        `Carbon Total Organic`, `Limit Carbon Total Organic`, `Carbon Total`,
        `Limit Carbon Total`, `Chlorophyll A`, `Limit Chlorophyll A`, `Nitrate (NO3) Dissolved`,
        `Limit Nitrate (NO3) Dissolved`, `Nitrate(NO3) + Nitrite(NO2) Dissolved`,
        `Limit Nitrate(NO3) + Nitrite(NO2) Dissolved`, `Nitrogen - Nitrite Dissolved (NO2)`,
        `Limit Nitrogen - Nitrite Dissolved (NO2)`, `Nitrogen Ammonia Total`,
        `Limit Nitrogen Ammonia Total`, `Nitrogen Total`,
        `Limit Nitrogen Total`, `Phosphorus Ort.Dis-P`, `Limit Phosphorus Ort.Dis-P`,
        `Phosphorus Total Dissolved`, `Limit Phosphorus Total Dissolved`, `Phosphorus Total`,
        `Limit Phosphorus Total`, `pH`, `Limit pH`, `Silica Reactive Diss`,
        `Limit Silica Reactive Diss`, `Turbidity`, `Limit Turbidity`, row_id
      )
  } else if (analysis_type == "metals") {
    params_metals <- params$PARAMETER[params$Comment == "metals"]

    data %<>% filter(PARAMETER %in% params_metals) %>%
      mutate(
        PARAMETER = paste0(PARAMETER, "unit:", UNIT),
        REQUISITION_ID = as.numeric(REQUISITION_ID)
      ) %>%
      select(-UNIT) %>%
      group_by_at(vars(-RESULT)) %>%
      mutate(row_id = seq_len(n())) %>%
      ungroup() %>%
      tidyr::spread(key = PARAMETER, value = RESULT, fill = NA) %>%
      arrange(COLLECTION_START)

    if (!"Hydroxide Alkalinityunit:mg/L" %in% names(data)) {
      data$`Hydroxide Alkalinityunit:mg/L` <- NA_real_
    }

    key_cols <- c(
      "SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
      "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH"
    )

    data %<>% clean_key_cols(key_cols) %>%
      group_by(
        SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
        ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH
      ) %>%
      mutate(ReplicateID = seq_len(n())) %>%
      ungroup()

    data %<>% add_ems_detection_limit_cols(params = params_metals)
    metals_units <- pull_ems_units(data)
    names(data) <- gsub("unit:.*", "", names(data))
    data %<>% map2_dfc(metals_units, fill_units)
    data %<>% select(
      SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID, ANALYZING_AGENCY,
      UPPER_DEPTH, LOWER_DEPTH, ReplicateID, `Alkalinity Phen. 8.3`,
      `Limit Alkalinity Phen. 8.3`, `Aluminum Dissolved`, `Limit Aluminum Dissolved`,
      `Aluminum Total`, `Limit Aluminum Total`, `Antimony Dissolved`,
      `Limit Antimony Dissolved`, `Antimony Total`, `Limit Antimony Total`,
      `Arsenic Dissolved`, `Limit Arsenic Dissolved`, `Arsenic Total`, `Limit Arsenic Total`,
      `Barium Dissolved`, `Limit Barium Dissolved`, `Barium Total`, `Limit Barium Total`,
      `Beryllium Dissolved`, `Limit Beryllium Dissolved`, `Beryllium Total`,
      `Limit Beryllium Total`, `Bicarbonate Alkalinity`, `Limit Bicarbonate Alkalinity`,
      `Bismuth Dissolved`, `Limit Bismuth Dissolved`, `Bismuth Total`, `Limit Bismuth Total`,
      `Boron Dissolved`, `Limit Boron Dissolved`, `Boron Total`, `Limit Boron Total`,
      `Cadmium Dissolved`, `Limit Cadmium Dissolved`, `Cadmium Total`, `Limit Cadmium Total`,
      `Calcium Dissolved`, `Limit Calcium Dissolved`, `Calcium Total`, `Limit Calcium Total`,
      `Carbonate Alkalinity`, `Limit Carbonate Alkalinity`, `Chromium Dissolved`,
      `Limit Chromium Dissolved`, `Chromium Total`, `Limit Chromium Total`,
      `Cobalt Dissolved`, `Limit Cobalt Dissolved`, `Cobalt Total`, `Limit Cobalt Total`,
      `Copper Dissolved`, `Limit Copper Dissolved`, `Copper Total`, `Limit Copper Total`,
      `Hardness (Dissolved)`, `Limit Hardness (Dissolved)`, `Hardness Total (Total)`,
      `Limit Hardness Total (Total)`, `Hydroxide Alkalinity`, `Limit Hydroxide Alkalinity`,
      `Iron Dissolved`, `Limit Iron Dissolved`, `Iron Total`, `Limit Iron Total`,
      `Lead Dissolved`, `Limit Lead Dissolved`, `Lead Total`, `Limit Lead Total`,
      `Magnesium Dissolved`, `Limit Magnesium Dissolved`, `Magnesium Total`,
      `Limit Magnesium Total`, `Manganese Dissolved`, `Limit Manganese Dissolved`,
      `Manganese Total`, `Limit Manganese Total`, `Molybdenum Dissolved`,
      `Limit Molybdenum Dissolved`, `Molybdenum Total`, `Limit Molybdenum Total`,
      `Nickel Dissolved`, `Limit Nickel Dissolved`, `Nickel Total`, `Limit Nickel Total`,
      `Phosphorus Total Dissolved metals`, `Limit Phosphorus Total Dissolved metals`,
      `Phosphorus Total metals`, `Limit Phosphorus Total metals`, `Potassium Dissolved`,
      `Limit Potassium Dissolved`, `Potassium Total`, `Limit Potassium Total`,
      `Selenium Dissolved`, `Limit Selenium Dissolved`, `Selenium Total`,
      `Limit Selenium Total`, `Silicon Dissolved`, `Limit Silicon Dissolved`,
      `Silicon Total`, `Limit Silicon Total`, `Silver Dissolved`, `Limit Silver Dissolved`,
      `Silver Total`, `Limit Silver Total`, `Sodium Dissolved`, `Limit Sodium Dissolved`,
      `Sodium Total`, `Limit Sodium Total`, `Strontium Dissolved`, `Limit Strontium Dissolved`,
      `Strontium Total`, `Limit Strontium Total`, `Sulfur Dissolved`,
      `Limit Sulfur Dissolved`, `Sulfur Total`, `Limit Sulfur Total`, `Thallium Dissolved`,
      `Limit Thallium Dissolved`, `Thallium Total`, `Limit Thallium Total`, `Tin Dissolved`,
      `Limit Tin Dissolved`, `Tin Total`, `Limit Tin Total`, `Titanium Dissolved`,
      `Limit Titanium Dissolved`, `Titanium Total`, `Limit Titanium Total`,
      `Uranium Dissolved`, `Limit Uranium Dissolved`, `Uranium Total`, `Limit Uranium Total`,
      `Vanadium Dissolved`, `Limit Vanadium Dissolved`, `Vanadium Total`,
      `Limit Vanadium Total`, `Zinc Dissolved`, `Limit Zinc Dissolved`, `Zinc Total`,
      `Limit Zinc Total`, row_id
    )
  }

  data %<>% mutate(
    LOWER_DEPTH = units::set_units(as.numeric(LOWER_DEPTH), "m"),
    UPPER_DEPTH = units::set_units(as.numeric(UPPER_DEPTH), "m")
  ) %>%
    select(
      SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
      ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH, ReplicateID,
      everything(), -row_id
    )
  data
}

add_ems_detection_limit_cols <- function(data, params, sep = "/") {
  for (name in names(data)) {
    base_name <- gsub("unit:.*", "", name)
    if (grepl("limit", name)) {
      next()
    } else if (base_name %in% params) {
      data %<>% tidyr::separate(name, c(paste("value", name), paste("Limit", name)), sep = sep) %>%
        mutate_at(vars(paste("Limit", name)), list(~ if_else(eval(parse(text = paste0("`", "Limit ", name, "`"))) == "<",
          eval(parse(text = paste0("`", "Limit ", name, "`"))),
          NA_character_
        ))) %>%
        mutate_at(
          vars(paste("Limit", name)),
          list(~ if_else(eval(parse(text = paste0("`", "Limit ", name, "`"))) == "<",
            eval(parse(text = paste0("`", "value ", name, "`"))),
            eval(parse(text = paste0("`", "Limit ", name, "`")))
          ))
        ) %>%
        mutate_at(
          vars(paste("value", name)),
          list(~ if_else(!is.na(eval(parse(text = paste0("`", "Limit ", name, "`")))),
            0, as.numeric(eval(parse(text = paste0("`", "value ", name, "`"))))
          ))
        ) %>%
        mutate_at(vars(paste("Limit", name)), list(~ as.numeric(eval(parse(text = paste0("`", "Limit ", name, "`"))))))
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
nrp_upload_ems_standard <- function(data, db_path = getOption("nrp.db_path", file.choose()), commit = TRUE,
                                    strict = TRUE, silent = TRUE, replace = FALSE) {
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_standard_data(data, exclusive = TRUE, order = TRUE)

  if (replace == FALSE) {
    query <- "SELECT COLLECTION_START FROM standardEMS WHERE COLLECTION_START = (SELECT MAX(COLLECTION_START) FROM standardEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)

    if (!nrow(result) == 0) {
      last_date <- result[[1, 1]]
      date_4yr <- dttr2::dtt_date_time(as.numeric(last_date) - 126316800, tz = "Etc/GMT+8")
      query <- paste0(
        "SELECT * FROM standardEMS WHERE ((`COLLECTION_START` >= '", date_4yr, "') AND (`COLLECTION_START` <= '",
        last_date, "'))"
      )
      ems_4year <- readwritesqlite::rws_query(query = query, conn = conn)

      data %<>% setdiff(ems_4year) %>%
        filter(COLLECTION_START > last_date)

      if (nrow(data) == 0) {
        err("The data you are attempting to upload is already in the database")
      } else {
        message(paste(
          "Trimmed new data.", nrow(data), "new rows to be included in upload for date range",
          first(data$COLLECTION_START), "-", last(data$COLLECTION_START)
        ))
      }
    }
  }

  readwritesqlite::rws_write(
    x = data, commit = commit, strict = strict, silent = silent,
    replace = replace, x_name = "standardEMS", conn = conn
  )
}

# rws_read_table("StandardEMS" , conn = conn)

#' Upload EMS metals data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
nrp_upload_ems_metals <- function(data, db_path = getOption("nrp.db_path", file.choose()), commit = TRUE,
                                  strict = TRUE, silent = TRUE, replace = FALSE) {
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_ems_metals_data(data, exclusive = TRUE, order = TRUE)

  if (replace == FALSE) {
    query <- "SELECT COLLECTION_START FROM metalsEMS WHERE COLLECTION_START = (SELECT MAX(COLLECTION_START) FROM metalsEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)

    if (!nrow(result) == 0) {
      last_date <- result[[1, 1]]
      date_4yr <- dttr2::dtt_date_time(as.numeric(last_date) - 126316800, tz = "Etc/GMT+8")
      query <- paste0(
        "SELECT * FROM metalsEMS WHERE ((`COLLECTION_START` >= '", date_4yr, "') AND (`COLLECTION_START` <= '",
        last_date, "'))"
      )
      ems_4year <- readwritesqlite::rws_query(query = query, conn = conn)

      data %<>% setdiff(ems_4year) %>%
        filter(COLLECTION_START > last_date)

      if (nrow(data) == 0) {
        err("The data you are attempting to upload is already in the database")
      } else {
        message(paste(
          "Trimmed new data.", nrow(data), "new rows to be included in upload for date range",
          first(data$COLLECTION_START), "-", last(data$COLLECTION_START)
        ))
      }
    }
  }
  readwritesqlite::rws_write(
    x = data, commit = commit, strict = strict, silent = silent,
    replace = replace, x_name = "metalsEMS", conn = conn
  )
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
                             show_detection_limits = FALSE) {
  chk::chk_null_or(start_date_time, chk = check_chr_date)
  chk::chk_null_or(end_date_time, chk = check_chr_date)
  chk::chk_chr(analysis_type)
  chk::chk_null_or(sites, chk = chk::chk_character)
  chk::chk_flag(show_detection_limits)

  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  site_table <- nrp::emsSites

  if (is.null(sites)) {
    sites <- site_table$SiteID
  }
  if (!all(sites %in% site_table$SiteID)) {
    err(paste("1 or more invalid site names"))
  }

  Date <- NULL
  SiteID <- NULL

  if (analysis_type == "standard") {
    table <- "standardEMS"
  } else if (analysis_type == "metals") {
    table <- "metalsEMS"
  }

  dates <- fill_date_time_query(
    table = table, col = "COLLECTION_START", end = end_date_time, start = start_date_time,
    connection = conn
  )
  start_date <- dates["start_date_time"][[1]]
  end_date <- dates["end_date_time"][[1]]

  if (start_date > end_date) {
    err("start date is later than end date")
  }

  sitesSql <- cc(sites, ellipsis = 1000)
  start_dateSql <- paste0("'", start_date_time, "'")
  end_dateSql <- paste0("'", end_date_time, "'")

  query <- paste0(
    "SELECT * FROM ", table, " WHERE ((`COLLECTION_START` >= ", start_dateSql, ") AND (`COLLECTION_START` <= ",
    end_dateSql, ") AND (`SiteID` IN (", sitesSql, ")))"
  )

  result <- readwritesqlite::rws_query(query = query, conn = conn) %>%
    dplyr::mutate(
      COLLECTION_START = dttr2::dtt_date_time(COLLECTION_START),
      COLLECTION_END = dttr2::dtt_date_time(COLLECTION_END)
    )

  if (show_detection_limits == FALSE) {
    result %<>% select(-c(grep("Limit", names(result))))
  }

  result
}

clean_key_cols <- function(data, cols) {
  cleaned_cols <- stats::complete.cases(data[, cols])
  data[cleaned_cols, ]
}
