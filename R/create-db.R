#' Create NRP SQLite Database
#'
#' @param path A string of the path to the database to create.
#' It must end with extension '.sqlite'.
#' @param ask A flag specifying whether to ask before deleting an existing file.
#' @return The path to the database.
#' @export
nrp_create_db <- function(path, ask = getOption("nrp.ask", TRUE)) {
  check_ext(path)
  check_flag(ask)

  if(file_exists(path)) {
    if(ask && !yesno("Really delete file '", path, "'?"))
      return(path)
    file_delete(path)
  }

conn <- readwritesqlite::rws_open_connection(path)

sites <- nrp::sites
readwritesqlite::rws_write_sqlite(sites, exists = F, conn = conn, x_name = "sites")

lakes <- nrp::lakes
readwritesqlite::rws_write_sqlite(lakes, exists = F, conn = conn, x_name = "lakes")

ctd <- initialize_ctd()
readwritesqlite::rws_write_sqlite(ctd, exists = F, conn = conn, x_name = "CTD")

readwritesqlite::rws_close_connection(conn = conn)
}


