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

#' Read phytoplankton raw data file
#'
#' @param path A string of the path to the file.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @return A tibble
#' @export
#'
nrp_read_phyto_file <- function(path, db_path = getOption("nrp.db_path",
                                                          file.choose())) {

  check_file_exists(path)

  if(!inherits(db_path, "SQLiteConnection")){
    db_path <- connect_if_valid_path(path = db_path)
    on.exit(readwritesqlite::rws_disconnect(conn = db_path))
  }

  data <- try(readxl::read_excel(path, col_types = "text"), silent = TRUE)

  if(inherits(data, "try-error")){
    err("Please ensure input data is a valid excel spreadsheet (.xlsx).")
  }

  data %<>% filter(!all_na(data)) %>%
    mutate(FileName = basename(path)) %>%
    clean_input_cols(lookup = nrp::phyto_input_cols) %>%
    transmute(
      Samp_Date = .data$Samp_Date, .data$Site_Name,
      SiteLoc_LocName = str_replace(.data$SiteLoc_LocName, "-", ""),
      Samp_Depth = str_replace(tolower(.data$Samp_Depth), "m", ""),
      .data$Class_Name, .data$Class_Alias,
      Species_Name = str_replace_all(.data$Species_Name, "\\.", ""),
      .data$Count_Number, .data$`NCU/mL`, .data$Species_Bvol,
      .data$`Biovolume (mm3/L)`, .data$Biomass, .data$Edibility, .data$FileName
    )

  sites <- nrp_download_sites(db_path = db_path)
  if(!all(unique(data$SiteLoc_LocName) %in% sites$SiteID)) {
    unknown <- unique(data$SiteLoc_LocName)[!unique(data$SiteLoc_LocName) %in% sites$SiteID]
    warning("Sites in input data not present in 'Sites' table in database: ", paste_vec(unknown), ".")
  }

  species <- nrp_download_phyto_species(db_path = db_path)
  if(!all(unique(data$Species_Name) %in% species$Taxa)){
    unknown <- unique(data$Species_Name)[!unique(data$Species_Name) %in% species$Taxa]
    warning("Taxa in input data not present in 'PhytoplanktonSpecies' table in database: ", paste_vec(unknown), ".")
  }

   data

}

#' Read phytoplankton raw data files
#'
#' @param path A string of the path to the directory.
#' @param db_path The SQLite connection object or path to the nrp SQLite database.
#' @inheritParams fs::dir_ls
#' @return A tibble.
#' @export
#'
nrp_read_phyto <- function(path = ".", db_path = getOption("nrp.db_path", file.choose()),
                           recursive = FALSE, regexp = "[.]xlsx$", fail = TRUE) {

  check_dir_exists(path)
  chk::chk_chr(regexp)
  chk::chk_flag(recursive)
  chk::chk_flag(fail)

  paths <- dir_ls(path, type = "file", recurse = recursive, regexp = regexp,
                  fail = fail)
  if(!length(paths)) return(named_list())

  datas <- suppressWarnings(do.call("rbind", map(paths, ~ nrp_read_phyto_file(., db_path = db_path))))
  rownames(datas) <- NULL

  datas
}

#' Upload phyto data to the nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An SQLite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'
db_path <- nrp_create_db(path = ":memory:", ask = FALSE)
path <- system.file(
  "extdata", "phyto", package = "nrp", mustWork = TRUE
)
data <- nrp_read_phyto(path, db_path = db_path)
commit = TRUE; strict = TRUE; silent = TRUE;
replace = FALSE


nrp_upload_phyto <- function(data, db_path = getOption("nrp.db_path", file.choose()),
                             commit = TRUE, strict = TRUE, silent = TRUE,
                             replace = FALSE){
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_phyto_raw_data(data, exclusive = FALSE, order = FALSE)
  chk::check_key(data, key = c("Samp_Date", "SiteLoc_LocName", "Samp_Depth", "Species_Name"))

  phyto_sample <- select(
    data, Date = .data$Samp_Date, SiteID = .data$SiteLoc_LocName,
    Depth = .data$Samp_Depth, .data$FileName
  ) %>%
    distinct()

  readwritesqlite::rws_write(x = phyto_sample, commit = commit,
                             strict = strict, silent = silent,
                             x_name = "PhytoplanktonSample", conn = conn, replace = replace)

  phyto_data <- select(
    data, Date = .data$Samp_Date, SiteID = .data$SiteLoc_LocName,
    Depth = .data$Samp_Depth, Taxa = .data$Species_Name,
    CellCount = .data$Count_Number, Abundance = .data$`NCU/mL`,
    SpeciesBvol = .data$Species_Bvol, Biovolume = .data$`Biovolume (mm3/L)`,
    .data$Biomass
  )

  readwritesqlite::rws_write(x = phyto_data, commit = commit, strict = strict,
                             silent = silent,
                             x_name = "Phytoplankton", conn = conn, replace = replace)
}
