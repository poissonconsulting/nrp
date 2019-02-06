#' Read CTD File
#'
#' @param path A string of the path to the file
#' @return A tibble
#' @export
#'
#' @examples
#' path <- system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
#'              package = "nrp")
#' nrp_read_ctd(path)
nrp_read_ctd <- function(path) {
  check_file_exists(path)
  ctd <- read.ctd.sbe(path, type="SBE19plus")
  data <- as_tibble(ctd@data)
  data$DateTime <- ctd@metadata$startTime
  # we need to get units from metadata
  # and we need to check data
  attr(data, "path") <- ctd@metadata$filename
  attr(data, "flob") <- flobr::flob(path)
  data
}
