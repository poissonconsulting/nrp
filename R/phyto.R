#' Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the database
#' @param data a tibble or data frame of new species data.
#' Must have columns "Taxa", "Genus", "ClassName", "ClassAlias". "ClassAlias" can be NA.
#' @param db_path The SQLite connection object or path to the SQLite database
#' @export
#'
nrp_add_phyto_species <- function(data, db_path = getOption("nrp.db_path", file.choose())) {
  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_new_phyto_species(data)

  readwritesqlite::rws_write(
    x = data, commit = TRUE, strict = TRUE, silent = TRUE,
    x_name = "PhytoplanktonSpecies", conn = conn
  )
}

#' Download phytoplankton species table
#' @param db_path The SQLite connection object or path to the SQLite database
#' @return The phytoplankton species table
#' @export
#'
nrp_download_phyto_species <- function(db_path = getOption("nrp.db_path", file.choose())) {
  conn <- db_path

  if (!inherits(conn, "SQLiteConnection")) {
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
nrp_read_phyto_file <- function(path, db_path = getOption(
                                  "nrp.db_path",
                                  file.choose()
                                )) {
  check_file_exists(path)

  if (!inherits(db_path, "SQLiteConnection")) {
    db_path <- connect_if_valid_path(path = db_path)
    on.exit(readwritesqlite::rws_disconnect(conn = db_path))
  }

  data <- try(readxl::read_excel(path, col_types = "text"), silent = TRUE)

  if (inherits(data, "try-error")) {
    err("Please ensure input data is a valid excel spreadsheet (.xlsx).")
  }

  data %<>% filter(!all_na(data)) %>%
    mutate(FileName = basename(path)) %>%
    clean_input_cols(lookup = nrp::phyto_input_cols) %>%
    transmute(
      Samp_Date = Samp_Date, Site_Name,
      SiteLoc_LocName = str_replace(SiteLoc_LocName, "-", ""),
      Samp_Depth = str_replace(tolower(Samp_Depth), "m", ""),
      Class_Name, Class_Alias,
      Species_Name = str_replace_all(Species_Name, "\\.", ""),
      Count_Number, `NCU/mL`, Species_Bvol,
      `Biovolume (mm3/L)`, Biomass, Edibility, FileName
    )

  sites <- nrp_download_sites(db_path = db_path)
  if (!all(unique(data$SiteLoc_LocName) %in% sites$SiteID)) {
    unknown <- unique(data$SiteLoc_LocName)[!unique(data$SiteLoc_LocName) %in% sites$SiteID]
    warning("Sites in input data not present in 'Sites' table in database: ", paste_vec(unknown), ".")
  }

  species <- nrp_download_phyto_species(db_path = db_path)
  if (!all(unique(data$Species_Name) %in% species$Taxa)) {
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

  paths <- dir_ls(path,
    type = "file", recurse = recursive, regexp = regexp,
    fail = fail
  )
  if (!length(paths)) {
    return(named_list())
  }

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
nrp_upload_phyto <- function(data, db_path = getOption("nrp.db_path", file.choose()),
                             commit = TRUE, strict = TRUE, silent = TRUE,
                             replace = FALSE) {
  chk::chk_flag(replace)
  chk::chk_flag(commit)
  chk::chk_flag(strict)
  chk::chk_flag(silent)

  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  check_phyto_raw_data(data, exclusive = FALSE, order = FALSE)
  chk::check_key(data, key = c("Samp_Date", "SiteLoc_LocName", "Samp_Depth", "Species_Name"))

  species <- readwritesqlite::rws_read_table("PhytoplanktonSpecies", conn = conn)

  data$Species_Name[1] <- "New"

  if (!all(data$Species_Name %in% species$Taxa)) {
    msg <- paste(
      "New species present in input data. Do you want to add the following species to the species table? ",
      data$Species_Name[!data$Species_Name %in% species$Taxa],
      collapse = ", "
    )

    spp_add <- ask_user(msg)

    if (!spp_add) err("Upload aborted.")

    new_species <- data %>%
      filter(!Species_Name %in% species$Taxa)

    if (!"Genus" %in% names(new_species)) new_species$Genus <- NA_character_

    new_species %<>%
      transmute(
        Taxa = Species_Name, Genus,
        ClassName = Class_Name, ClassAlias = Class_Alias
      ) %>%
      distinct()

    nrp_add_phyto_species(new_species, db_path = conn)
  }

  phyto_sample <- select(
    data,
    Date = Samp_Date, SiteID = SiteLoc_LocName,
    Depth = Samp_Depth, FileName
  ) %>%
    distinct()

  readwritesqlite::rws_write(
    x = phyto_sample, commit = commit,
    strict = strict, silent = silent,
    x_name = "PhytoplanktonSample", conn = conn, replace = replace
  )

  phyto_data <- select(
    data,
    Date = Samp_Date, SiteID = SiteLoc_LocName,
    Depth = Samp_Depth, Taxa = Species_Name,
    CellCount = Count_Number, Abundance = `NCU/mL`,
    SpeciesBvol = Species_Bvol, Biovolume = `Biovolume (mm3/L)`,
    Biomass
  )

  readwritesqlite::rws_write(
    x = phyto_data, commit = commit, strict = strict,
    silent = silent,
    x_name = "Phytoplankton", conn = conn, replace = replace
  )
}

#' Download Phytoplankton data table from database
#'
#' @param start_date The start date
#' @param end_date The end date
#' @param sites A character vector of the Site IDs
#' @param species A character vector of the species to include. Defaults to 'all'
#' Permissible values can be found in the PhytoplanktonSpecies table.
#' @param db_path The SQLite connection object or path to the SQLite database
#'
#' @return Phytoplankton data table
#' @export
#'
nrp_download_phyto <- function(start_date = NULL, end_date = NULL,
                               sites = NULL, species = "all",
                               db_path = getOption("nrp.db_path", file.choose())) {
  chk::chk_null_or(sites, chk = chk::chk_character)
  chk::chk_character(species)
  chk::chk_null_or(start_date, chk = check_chr_date)
  if (!is.null(start_date)) {
    check_chr_date(end_date)
  }
  conn <- db_path
  if (!inherits(conn, "SQLiteConnection")) {
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  site_table <- nrp_download_sites(db_path = conn)
  if (is.null(sites)) {
    sites <- site_table$SiteID
  }
  if (!all(sites %in% site_table$SiteID)) {
    err(paste("1 or more invalid site names"))
  }

  species_db <- readwritesqlite::rws_read_table("PhytoplanktonSpecies", conn = conn)

  if (identical(species, "all")) {
    species <- species_db$Taxa
  } else if (!all(species %in% species_db$Taxa)) {
    wrong_spp <- species[!species %in% species_db$Taxa]
    err(paste("Unrecognized species in query: ", wrong_spp, collapse = ", "))
  }

  dates <- fill_date_query(
    table = "Phytoplankton", col = "Date", end = end_date, start = start_date,
    connection = conn
  )

  start_date <- dates["start_date"][[1]]
  end_date <- dates["end_date"][[1]]

  if (start_date > end_date) {
    err("start date is later than end date")
  }

  Date <- NULL
  SiteID <- NULL
  speciesSql <- cc(species, ellipsis = 1000)
  sitesSql <- cc(sites, ellipsis = 1000)
  start_dateSql <- paste0("'", start_date, "'")
  end_dateSql <- paste0("'", end_date, "'")

  query <- paste0(
    "SELECT * FROM Phytoplankton WHERE ((`Date` >= ", start_dateSql, ") AND (`Date` <= ",
    end_dateSql, ") AND (`SiteID` IN (", sitesSql, ")) AND (`Taxa` IN (", speciesSql, ")))"
  )

  result <- readwritesqlite::rws_query(query = query, conn = conn, meta = TRUE) %>%
    dplyr::mutate(Date = dttr2::dtt_date(Date))

  if (nrow(result) == 0) warning("no data available for query provided.")
  result
}
