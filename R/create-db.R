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
                FileID INTEGER NOT NULL,
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

suppressWarnings(DBI::dbGetQuery(conn,
                                 "CREATE TABLE SitesEMS  (
                                 SiteID TEXT NOT NULL,
                                 EmsSite TEXT NOT NULL,
                                 SiteName TEXT NOT NULL,
                                 geometry BLOB,
                                 PRIMARY KEY (SiteID))"))

suppressWarnings(DBI::dbGetQuery(conn,
                                 "CREATE TABLE standardEMS  (
                                 RowID INTEGER NOT NULL,
                                 SiteID TEXT NOT NULL,
                                 COLLECTION_START TEXT NOT NULL,
                                 COLLECTION_END TEXT NOT NULL,
                                 REQUISITION_ID REAL NOT NULL,
                                 ANALYZING_AGENCY TEXT NOT NULL,
                                 UPPER_DEPTH REAL,
                                 LOWER_DEPTH REAL,
                                 [Alkalinity Total 4.5] REAL,
                                 [Limit Alkalinity Total 4.5] REAL,
                                 [Carbon Total Inorganic] REAL,
                                 [Limit Carbon Total Inorganic] REAL,
                                 [Carbon Total Organic] REAL,
                                 [Limit Carbon Total Organic] REAL,
                                 [Carbon Total] REAL,
                                 [Limit Carbon Total] REAL,
                                 [Nitrate (NO3) Dissolved] REAL,
                                 [Limit Nitrate (NO3) Dissolved] REAL,
                                 [Nitrate(NO3) + Nitrite(NO2) Dissolved] REAL,
                                 [Limit Nitrate(NO3) + Nitrite(NO2) Dissolved] REAL,
                                 [Nitrogen - Nitrite Dissolved (NO2)] REAL,
                                 [Limit Nitrogen - Nitrite Dissolved (NO2)] REAL,
                                 [Nitrogen Ammonia Total] REAL,
                                 [Limit Nitrogen Ammonia Total] REAL,
                                 [Nitrogen Total] REAL,
                                 [Limit Nitrogen Total] REAL,
                                 [Phosphorus Ort.Dis-P] REAL,
                                 [Limit Phosphorus Ort.Dis-P] REAL,
                                 [Phosphorus Total Dissolved] REAL,
                                 [Limit Phosphorus Total Dissolved] REAL,
                                 [Phosphorus Total] REAL,
                                 [Limit Phosphorus Total] REAL,
                                 [pH] REAL,
                                 [Limit pH] REAL,
                                 [Silica Reactive Diss] REAL,
                                 [Limit Silica Reactive Diss] REAL,
                                 [Turbidity] REAL,
                                 [Limit Turbidity] REAL,
                                 PRIMARY KEY (RowID))"))


suppressWarnings(DBI::dbGetQuery(conn,
                                 "CREATE TABLE metalsEMS  (
                                 RowID INTEGER NOT NULL,
                                 SiteID TEXT NOT NULL,
                                 COLLECTION_START TEXT NOT NULL,
                                 COLLECTION_END TEXT NOT NULL,
                                 REQUISITION_ID REAL NOT NULL,
                                 ANALYZING_AGENCY TEXT NOT NULL,
                                 UPPER_DEPTH REAL,
                                 LOWER_DEPTH REAL,
                                 [Alkalinity Phen. 8.3] REAL,
                                 [Limit Alkalinity Phen. 8.3] REAL,
                                 [Aluminum Dissolved] REAL,
                                 [Limit Aluminum Dissolved] REAL,
                                 [Aluminum Total] REAL,
                                 [Limit Aluminum Total] REAL,
                                 [Antimony Dissolved] REAL,
                                 [Limit Antimony Dissolved] REAL,
                                 [Antimony Total] REAL,
                                 [Limit Antimony Total] REAL,
                                 [Arsenic Dissolved] REAL,
                                 [Limit Arsenic Dissolved] REAL,
                                 [Arsenic Total] REAL NULL,
                                 [Limit Arsenic Total] REAL,
                                 [Barium Dissolved] REAL,
                                 [Limit Barium Dissolved] REAL,
                                 [Barium Total] REAL,
                                 [Limit Barium Total] REAL,
                                 [Beryllium Dissolved] REAL,
                                 [Limit Beryllium Dissolved] REAL,
                                 [Beryllium Total] REAL,
                                 [Limit Beryllium Total] REAL,
                                 [Bicarbonate Alkalinity] REAL,
                                 [Limit Bicarbonate Alkalinity] REAL,
                                 [Bismuth Dissolved] REAL,
                                 [Limit Bismuth Dissolved] REAL,
                                 [Bismuth Total] REAL,
                                 [Limit Bismuth Total] REAL,
                                 [Boron Dissolved] REAL,
                                 [Limit Boron Dissolved] REAL,
                                 [Boron Total] REAL,
                                 [Limit Boron Total] REAL,
                                 [Cadmium Dissolved] REAL,
                                 [Limit Cadmium Dissolved] REAL,
                                 [Cadmium Total] REAL,
                                 [Limit Cadmium Total] REAL,
                                 [Calcium Dissolved] REAL,
                                 [Limit Calcium Dissolved] REAL,
                                 [Calcium Total] REAL,
                                 [Limit Calcium Total] REAL,
                                 [Carbonate Alkalinity] REAL,
                                 [Limit Carbonate Alkalinity] REAL,
                                 [Chromium Dissolved] REAL,
                                 [Limit Chromium Dissolved] REAL,
                                 [Chromium Total] REAL,
                                 [Limit Chromium Total] REAL,
                                 [Cobalt Dissolved] REAL,
                                 [Limit Cobalt Dissolved] REAL,
                                 [Cobalt Total] REAL,
                                 [Limit Cobalt Total] REAL,
                                 [Copper Dissolved] REAL,
                                 [Limit Copper Dissolved] REAL,
                                 [Copper Total] REAL,
                                 [Limit Copper Total] REAL,
                                 [Hardness (Dissolved)] REAL,
                                 [Limit Hardness (Dissolved)] REAL,
                                 [Hardness Total (Total)] REAL,
                                 [Limit Hardness Total (Total)] REAL,
                                 [Hydroxide Alkalinity] REAL,
                                 [Limit Hydroxide Alkalinity] REAL,
                                 [Iron Dissolved] REAL,
                                 [Limit Iron Dissolved] REAL,
                                 [Iron Total] REAL,
                                 [Limit Iron Total] REAL,
                                 [Lead Dissolved] REAL,
                                 [Limit Lead Dissolved] REAL,
                                 [Lead Total] REAL,
                                 [Limit Lead Total] REAL,
                                 [Magnesium Dissolved] REAL,
                                 [Limit Magnesium Dissolved] REAL,
                                 [Magnesium Total] REAL,
                                 [Limit Magnesium Total] REAL,
                                 [Manganese Dissolved] REAL,
                                 [Limit Manganese Dissolved] REAL,
                                 [Manganese Total] REAL,
                                 [Limit Manganese Total] REAL,
                                 [Molybdenum Dissolved] REAL,
                                 [Limit Molybdenum Dissolved] REAL,
                                 [Molybdenum Total] REAL,
                                 [Limit Molybdenum Total] REAL,
                                 [Nickel Dissolved] REAL,
                                 [Limit Nickel Dissolved] REAL,
                                 [Nickel Total] REAL,
                                 [Limit Nickel Total] REAL,
                                 [Phosphorus Total Dissolved metals] REAL,
                                 [Limit Phosphorus Total Dissolved metals] REAL,
                                 [Phosphorus Total metals] REAL,
                                 [Limit Phosphorus Total metals] REAL,
                                 [Potassium Dissolved] REAL,
                                 [Limit Potassium Dissolved] REAL,
                                 [Potassium Total] REAL,
                                 [Limit Potassium Total] REAL,
                                 [Selenium Dissolved] REAL,
                                 [Limit Selenium Dissolved] REAL,
                                 [Selenium Total] REAL,
                                 [Limit Selenium Total] REAL,
                                 [Silicon Dissolved] REAL,
                                 [Limit Silicon Dissolved] REAL,
                                 [Silicon Total] REAL,
                                 [Limit Silicon Total] REAL,
                                 [Silver Dissolved] REAL,
                                 [Limit Silver Dissolved] REAL,
                                 [Silver Total] REAL,
                                 [Limit Silver Total] REAL,
                                 [Sodium Dissolved] REAL,
                                 [Limit Sodium Dissolved] REAL,
                                 [Sodium Total] REAL,
                                 [Limit Sodium Total] REAL,
                                 [Strontium Dissolved] REAL,
                                 [Limit Strontium Dissolved] REAL,
                                 [Strontium Total] REAL,
                                 [Limit Strontium Total] REAL,
                                 [Sulfur Dissolved] REAL,
                                 [Limit Sulfur Dissolved] REAL,
                                 [Sulfur Total] REAL,
                                 [Limit Sulfur Total] REAL,
                                 [Thallium Dissolved] REAL,
                                 [Limit Thallium Dissolved] REAL,
                                 [Thallium Total] REAL,
                                 [Limit Thallium Total] REAL,
                                 [Tin Dissolved] REAL,
                                 [Limit Tin Dissolved] REAL,
                                 [Tin Total] REAL,
                                 [Limit Tin Total] REAL,
                                 [Titanium Dissolved] REAL,
                                 [Limit Titanium Dissolved] REAL,
                                 [Titanium Total] REAL,
                                 [Limit Titanium Total] REAL,
                                 [Uranium Dissolved] REAL,
                                 [Limit Uranium Dissolved] REAL,
                                 [Uranium Total] REAL,
                                 [Limit Uranium Total] REAL,
                                 [Vanadium Dissolved] REAL,
                                 [Limit Vanadium Dissolved] REAL,
                                 [Vanadium Total] REAL,
                                 [Limit Vanadium Total] REAL,
                                 [Zinc Dissolved] REAL,
                                 [Limit Zinc Dissolved] REAL,
                                 [Zinc Total] REAL,
                                 [Limit Zinc Total] REAL,
                                 PRIMARY KEY (RowID))"))


lakes <- nrp::lakes
readwritesqlite::rws_write(lakes, exists = TRUE, conn = conn, x_name = "Lake")

basinArm <- nrp::basinArm
readwritesqlite::rws_write(basinArm, exists = TRUE, conn = conn, x_name = "BasinArm")

sites <- nrp::sites
readwritesqlite::rws_write(sites, exists = TRUE, conn = conn, x_name = "sites")

visitCTD <- initialize_ctd_visit()
readwritesqlite::rws_write(visitCTD, exists = TRUE, conn = conn, x_name = "visitCTD")

ctd <- initialize_ctd()
readwritesqlite::rws_write(ctd, exists = TRUE, conn = conn, x_name = "CTD")

ems_sites <- nrp::emsSites
readwritesqlite::rws_write(ems_sites, exists = TRUE, conn = conn, x_name = "sitesEMS")

ems_metals <- nrp::ems_metals_init
readwritesqlite::rws_write(ems_metals, exists = TRUE, conn = conn, x_name = "metalsEMS")

ems_standard <- nrp::ems_standard_init
readwritesqlite::rws_write(ems_standard, exists = TRUE, conn = conn, x_name = "standardEMS")

conn
}
