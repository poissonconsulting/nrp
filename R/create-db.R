#' Create NRP SQLite Database
#'
#' @param path A string of the path to the database to create.
#' It must end with extension '.sqlite'.
#' @param ask A flag specifying whether to ask before deleting an existing file.
#' @return The path to the database.
#' @export
nrp_create_db <- function(path, ask = getOption("nrp.ask", TRUE)) {
  check_string(path)
  check_flag(ask)
  if(file_exists(path)) {
    if(ask && !yesno("Really delete file '", path, "'?"))
      return(path)
    file_delete(path)
  }

conn <- suppressWarnings(readwritesqlite::rws_open_connection(path))


DBI::dbGetQuery(conn,
                "CREATE TABLE CTD  (
                SiteID TEXT NOT NULL,
                DateTime TEXT NOT NULL,
                Depth REAL NOT NULL,
                Temperature REAL NOT NULL,
                Oxygen REAL NOT NULL,
                Oxygen2 REAL NOT NULL,
                Conductivity REAL NOT NULL,
                Conductivity2 REAL NOT NULL,
                Salinity REAL NOT NULL,
                Backscatter REAL NOT NULL,
                Fluorescence REAL NOT NULL,
                Frequency REAL NOT NULL,
                Flag BOOLEAN NOT NULL,
                Pressure INTEGER NOT NULL,
                FOREIGN KEY (SiteID) REFERENCES Sites (SiteID),
                PRIMARY KEY (SiteID, DateTime, Depth))")

DBI::dbGetQuery(conn,
                "CREATE TABLE Sites  (
                SiteID TEXT NOT NULL,
                SiteNumber REAL NOT NULL,
                SiteName TEXT NOT NULL,
                BasinArm TEXT NOT NULL,
                Depth REAL NOT NULL,
                geometry BLOB NOT NULL,
                FOREIGN KEY (BasinArm) REFERENCES Lakes (BasinArm),
                PRIMARY KEY (SiteID))")

DBI::dbGetQuery(conn,
                "CREATE TABLE Lakes  (
                Lake TEXT NOT NULL,
                BasinArm TEXT NOT NULL,
                PRIMARY KEY (BasinArm))")

lakes <- nrp::lakes
readwritesqlite::rws_write_sqlite(lakes, exists = T, conn = conn, x_name = "lakes")

sites <- nrp::sites
readwritesqlite::rws_write_sqlite(sites, exists = T, conn = conn, x_name = "sites")

ctd <- initialize_ctd()
readwritesqlite::rws_write_sqlite(ctd, exists = T, conn = conn, x_name = "CTD")

conn
}
