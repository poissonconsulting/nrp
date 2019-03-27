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
                FileID INTEGER NULL,
                SiteID TEXT NOT NULL,
                Date TEXT NOT NULL,
                Time TEXT NOT NULL,
                Depth REAL NOT NULL,
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
                Retain TEXT,
                FOREIGN KEY (SiteID, Date, Time) REFERENCES VisitCTD (SiteID, Date, Time),
                FOREIGN KEY (SiteID) REFERENCES Sites (SiteID),
                PRIMARY KEY (SiteID, Date, Time, Depth))"))


suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE VisitCTD  (
                 SiteID TEXT NOT NULL,
                 Date TEXT NOT NULL,
                 Time TEXT NOT NULL,
                 DepthDuplicates INTEGER NOT NULL,
                 File TEXT NOT NULL,
                 FOREIGN KEY (SiteID) REFERENCES Sites (SiteID),
                 PRIMARY KEY (SiteID, Date, Time))"))

suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE Sites  (
                SiteID TEXT NOT NULL,
                SiteNumber REAL,
                SiteName TEXT,
                BasinArm TEXT,
                Depth REAL,
                geometry BLOB,
                FOREIGN KEY (BasinArm) REFERENCES BasinArm (BasinArm),
                PRIMARY KEY (SiteID))"))

suppressWarnings(DBI::dbGetQuery(conn,
                "CREATE TABLE BasinArm  (
                Lake TEXT NOT NULL,
                BasinArm TEXT NOT NULL,
                FOREIGN KEY (Lake) REFERENCES Lake (Lake)
                PRIMARY KEY (BasinArm))"))

suppressWarnings(DBI::dbGetQuery(conn,
                                 "CREATE TABLE Lake  (
                                 Lake TEXT NOT NULL,
                                 Area REAL NOT NULL,
                                 geometry BLOB NOT NULL,
                                 PRIMARY KEY (Lake))"))
lakes <- nrp::lakes
readwritesqlite::rws_write(lakes, exists = TRUE, conn = conn, x_name = "Lake")

basinArm <- nrp::basinArm
readwritesqlite::rws_write(basinArm, exists = TRUE, conn = conn, x_name = "BasinArm")

sites <- nrp::sites
readwritesqlite::rws_write(sites, exists = TRUE, conn = conn, x_name = "sites")

ctd <- initialize_ctd()
readwritesqlite::rws_write(ctd, exists = TRUE, conn = conn, x_name = "CTD")

conn
}
