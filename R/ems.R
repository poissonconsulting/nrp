#' Extract and clean EMS data
#'
#' @param data The EMS data to be extracted. Data must first be downloaded from server by using rems package
#' @param analysis_type EMS data of interest. Must be either "standard" or "metals"
#' @return A a data frame
#' @export

# data <- nrp_extract_ems(data = ems, analysis_type = "standard")

nrp_extract_ems <- function(data, analysis_type = "standard"){
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
