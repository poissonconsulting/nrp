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
  # we will need to get units from metadata
  # note we may need to update check_ctd_data accordingly
  colnames(data) %<>% str_to_title()
  data$DateTime <- ctd@metadata$startTime
  data %<>% select(.data$DateTime, everything())
  check_ctd_data(data, exclusive = TRUE, order = TRUE)

  attr(data, "path") <- ctd@metadata$filename
  attr(data, "flob") <- flobr::flob(path)
  data
}

#' Read CTD Files
#'
#' @param path A string of the path to the directory.
#' @param bind A logical argument for whether to bind rows into one tibble or return as a list of tibbles
#' @inheritParams fs::dir_ls
#' @return A list of tibbles.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ctd/2018", package = "nrp")
#' nrp_read_ctd(path)
nrp_read_ctd <- function(path = ".", recursive = FALSE, bind = TRUE, regexp = "[.]cnv$",
                         fail = TRUE) {
  check_dir_exists(path)
  paths <- dir_ls(path, type = "file", recursive = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())
  if(bind == FALSE){
    datas <- purrr::map(paths, nrp_read_ctd_file)
  } else {
    datas <- purrr::map_dfr(paths, nrp_read_ctd_file)
  }
  datas
}

