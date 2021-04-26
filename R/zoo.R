#' Read zooplankton raw data file
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @param system The system 'arrow' or 'kootenay'. If null, the system is detected from the file name.
#' @return A tibble
#' @export
nrp_read_zooplankton_file <- function(path, db_path = getOption("nrp.db_path",
                                                                file.choose()), system = NULL) {
  check_file_exists(path)

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

  zoo_col_types <- c("text", "numeric", "numeric", "text",  "date",  "date",  "numeric",  "text",  "text",
                     "numeric", "text", "text", rep("numeric", 154), "text", "text", "text")

  data <- try(readxl::read_excel(path, col_types = zoo_col_types), silent = TRUE)

  if(inherits(data, "try-error")){
    err("Columns in data do not match template for zooplankton raw data. see `nrp::zoo_cols` for correct column names and order.")
  }
  check_zoo_raw_data(data)

  data %<>% filter(!all_na(data)) %>%
    mutate(Station = paste0(system, .data$Station))

  sites <- nrp_download_sites(db_path = db_path)

  if(!all(unique(data$Station) %in% sites$SiteID)) err("Unknown Stations in raw data.")
  chk::check_key(data, key = c("Date1", "Station", "Replicate", "FileName"))

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
  paths <- dir_ls(path, type = "file", recurse = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_zooplankton_file(., db_path = db_path, system = system))))
  rownames(datas) <- NULL

  datas
}
