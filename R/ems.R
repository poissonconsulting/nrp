#' Extract and clean EMS data
#'
#' @param data The EMS data to be extracted. Data must first be downloaded from server by using rems package
#' @param db_path The SQLite connection object or path to the SQLite database
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @return A a data frame
#' @export

# # data <- nrp_extract_ems(data = ems, analysis_type = "standard")
# path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
# # ems <- readRDS(path)
# conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
# db_path <- conn
# data <- ems
# analysis_type <- "metals"

nrp_extract_ems <- function(data, db_path, analysis_type = "standard"){

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  sites <- nrp::emsSites
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
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT)) %>%
      distinct() %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START) %>%
      select(.data$SiteID, .data$COLLECTION_START, everything(), -.data$row_id)
    data %<>% add_ems_detection_limit_cols(params = params_standard)
    standard_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(standard_units, fill_units)

    query <- "SELECT RowID FROM metalsEMS WHERE RowID = (SELECT MAX(RowID) FROM metalsEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)
    last_id <- result$RowID[1]
    if(is.na(last_id)) last_id <- 0

    data %<>% mutate(RowID = (last_id + 1):nrow(data)) %>%
      select(.data$RowID, everything())

  } else if(analysis_type == "metals"){
    params_metals <- params$PARAMETER[params$Comment == "metals"]

    data %<>% filter(.data$PARAMETER %in% params_metals) %>%
      mutate(PARAMETER = paste0(.data$PARAMETER,"unit:", .data$UNIT)) %>%
      distinct() %>%
      select(-.data$UNIT) %>%
      group_by_at(vars(-.data$RESULT)) %>%
      mutate(row_id = 1:n()) %>% ungroup() %>%
      tidyr::spread(key = .data$PARAMETER, value = .data$RESULT, fill = NA) %>%
      arrange(.data$COLLECTION_START) %>%
      select(.data$SiteID, .data$COLLECTION_START, everything(), -.data$row_id)
    data %<>% add_ems_detection_limit_cols(params = params_metals)
    metals_units <- pull_ems_units(data)
    names(data) <- gsub('unit:.*', "", names(data))
    data %<>% map2_dfc(metals_units, fill_units)

    query <- "SELECT RowID FROM metalsEMS WHERE RowID = (SELECT MAX(RowID) FROM metalsEMS);"
    result <- readwritesqlite::rws_query(query = query, conn = conn)
    last_id <- result$RowID[1]
    if(is.na(last_id)) last_id <- 0

    data %<>% mutate(RowID = (last_id + 1):nrow(data)) %>%
      select(.data$RowID, everything())

  } else {
    err("analysis_type must be either 'standard' or 'metals'")
  }
  # if(show_detection_limits = FALSE){
  #   data %<>% select_if(vars(contains("Limit")))
  # }
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
nrp_upload_ems_standard<- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  # visit <- group_by(data, .data$SiteID, .data$Date, .data$Time) %>%
  #   summarise(DepthDuplicates = length(which(.data$Retain == FALSE)), File = first(.data$File)) %>%
  #   ungroup()
  #
  # visit_db <- nrp_download_ctd_visit(db_path = conn)
  # visit_upload <- setdiff(visit, visit_db)
  #
  # readwritesqlite::rws_write(x = visit_upload, commit = commit, strict = strict, silent = silent,
  #                            x_name = "visitCTD", conn = conn)
  # n_pre_filt <- nrow(data)
  # data %<>% filter(.data$Retain == TRUE)
  # n_dups <- n_pre_filt - nrow(data)
  # message(paste(n_dups, "duplicate depths removed from data"))

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

  # visit <- group_by(data, .data$SiteID, .data$Date, .data$Time) %>%
  #   summarise(DepthDuplicates = length(which(.data$Retain == FALSE)), File = first(.data$File)) %>%
  #   ungroup()
  #
  # visit_db <- nrp_download_ctd_visit(db_path = conn)
  # visit_upload <- setdiff(visit, visit_db)
  #
  # readwritesqlite::rws_write(x = visit_upload, commit = commit, strict = strict, silent = silent,
  #                            x_name = "visitCTD", conn = conn)
  # n_pre_filt <- nrow(data)
  # data %<>% filter(.data$Retain == TRUE)
  # n_dups <- n_pre_filt - nrow(data)
  # message(paste(n_dups, "duplicate depths removed from data"))

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                             x_name = "metalsEMS", conn = conn)
}
