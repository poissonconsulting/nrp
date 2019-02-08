#' Upload data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path The oath to the database file
#' @inheritParams readwritesqlite::rws_write_sqlite
#'
#' @export
#'
nrp_upload <- function(data, db_path, commit = TRUE, strict = TRUE, silent = TRUE){

  conn <- readwritesqlite::rws_open_connection(db_path)
  on.exit(readwritesqlite::rws_close_connection(conn))

  readwritesqlite::rws_write_sqlite(data, commit = commit, strict = strict, silent = silent,
                                   x_name = assess_data_type(data), conn = conn)
}


