#' Upload data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param conn A formal SQlite connection
#' @inheritParams readwritesqlite::rws_write_sqlite
#'
#' @export
#'


nrp_upload <- function(data, conn){

  ctd_names <- c('DateTime', 'Depth', 'Temperature', 'Oxygen', 'Oxygen2', 'Conductivity', 'Conductivity2',
                 'Salinity', 'Backscatter', 'Fluorescence', 'Frequency', 'Flag', 'Pressure')

  if(identical(names(data), ctd_names)){
    check_ctd_data(data, exclusive = TRUE, order = TRUE)
 #   rws_write_sqlite(ctds, conn = conn, x_name = "CTD")
  } else {
    err("Columns names do not match any existing data in database.")
  }
}
