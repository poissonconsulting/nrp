#' Upload data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param conn A formal SQlite connection
#' @inheritParams readwritesqlite::rws_write_sqlite
#'
#' @export
#'
nrp_upload <- function(data, conn, commit = TRUE){

 rws_write_sqlite(data, commit = commit, conn = conn, x_name = assess_data_type(data))

}
