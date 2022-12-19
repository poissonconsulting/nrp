#' Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the database
#' @param data a tibble or data frame of new species data.
#' Must have columns "Taxa", "Genus", "ClassName", "ClassAlias". "ClassAlias" can be NA.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @export
#'
nrp_add_phyto_species <- function(data, db_path = getOption("nrp.db_path", file.choose())){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_new_phyto_species(data)

  readwritesqlite::rws_write(x = data, commit = TRUE, strict = TRUE, silent = TRUE,
                             x_name = "PhytoplanktonSpecies", conn = conn)
}

#' Download phytoplankton species table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return The phytoplankton species table
#' @export
#'
nrp_download_phyto_species <- function(db_path = getOption("nrp.db_path", file.choose())) {
  conn <- db_path

  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }
  readwritesqlite::rws_read_table("PhytoplanktonSpecies", conn = conn)
}
