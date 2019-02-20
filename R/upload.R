#' Upload CTD data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write_sqlite
#' @export
#'

nrp_upload_ctd <- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }
  readwritesqlite::rws_write_sqlite(x = data, commit = commit, strict = strict, silent = silent,
                                    x_name = "CTD", conn = conn)
}

