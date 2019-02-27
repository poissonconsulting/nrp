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


suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE CTD  (
                SiteID TEXT NOT NULL,
                DateTime TEXT,
                Depth REAL,
                Temperature REAL,
                Oxygen REAL,
                Oxygen2 REAL,
                Conductivity REAL,
                Conductivity2 REAL,
                Salinity REAL,
                Backscatter REAL,
                Fluorescence REAL,
                Frequency REAL,
                Flag REAL,
                Pressure INTEGER,
                FOREIGN KEY (SiteID) REFERENCES Sites (SiteID),
                PRIMARY KEY (SiteID, DateTime, Depth))"))

suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE Sites  (
                SiteID TEXT NOT NULL,
                SiteNumber REAL,
                SiteName TEXT,
                BasinArm TEXT,
                Depth REAL,
                geometry BLOB,
                FOREIGN KEY (BasinArm) REFERENCES Lakes (BasinArm),
                PRIMARY KEY (SiteID))"))

suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE Lakes  (
                Lake TEXT NOT NULL,
                BasinArm TEXT NOT NULL,
                PRIMARY KEY (BasinArm))"))

lakes <- nrp::lakes
readwritesqlite::rws_write_sqlite(lakes, exists = TRUE, conn = conn, x_name = "lakes")

sites <- nrp::sites
readwritesqlite::rws_write_sqlite(sites, exists = TRUE, conn = conn, x_name = "sites")

ctd <- initialize_ctd()
readwritesqlite::rws_write_sqlite(ctd, exists = TRUE, conn = conn, x_name = "CTD")

conn
}
