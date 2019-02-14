#' Upload data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param conn An Sqlite Database Connection
#' @inheritParams readwritesqlite::rws_write_sqlite
#'
#'
#'
nrp_upload_data <- function(data, conn, commit, strict, silent){

  readwritesqlite::rws_write_sqlite(x = data, commit = commit, strict = strict, silent = silent,
                                   x_name = assess_data_type(data), conn = conn)
}


#' Upload data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param conn An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write_sqlite
#' @export
#'
#'
nrp_upload <- function(data, conn, commit = TRUE, strict = TRUE, silent = TRUE){

  if(!class(conn) == "SQLiteConnection"){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }
  nrp_upload_data(data = data, conn = conn, commit = commit, strict = strict, silent = silent)
}


