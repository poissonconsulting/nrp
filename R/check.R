check_path <- function(path) {
  chk_string(path)
  path
}

check_ext <- function(path, ext = "sqlite") {
  check_path(path)
  if(!identical(path_ext(path), ext))
    err("path '", path, "' extension must be '", ext, "',")
  path
}

check_file_exists <- function(path) {
  check_path(path)
  if(!file_exists(path)) err("path '", path, "' must exist")
  if(!is_file(path)) err("path '", path, "' must be a file")
  path
}

check_dir_exists <- function(path) {
  check_path(path)
  if(!file_exists(path)) err("path '", path, "' must exist")
  if(!is_dir(path)) err("path '", path, "' must be a directory")
  path
}

check_ctd_data <- function(data, exclusive = FALSE, order = FALSE) {
  check_data(
    data,
    values = list(
      FileID = as.integer(),
      SiteID = "character",
      Date = as.Date("2018-01-01"),
      Time = dttr2::dtt_time(c(NA_real_, "00:00:00")),
      Depth = units::as_units(c(NA, 1), "m"),
      Temperature = units::as_units(c(NA, 1), "degC"),
      Oxygen = units::as_units(c(NA, 1), "mg/l"),
      Oxygen2 = units::as_units(c(NA, 1), "percent"),
      Conductivity = units::as_units(c(NA, 1), "uS/cm"),
      Conductivity2 = units::as_units(c(NA, 1), "mu * S/cm"),
      Salinity = units::as_units(c(NA, 1), "PSU"),
      Backscatter = units::as_units(c(NA, 1), "NTU"),
      Fluorescence = units::as_units(c(NA, 1), "ug/L"),
      Frequency = units::as_units(c(NA, 1), "Hz"),
      Flag = 1,
      Pressure = units::as_units(c(NA, 1), "dbar"),
      Retain = TRUE,
      File = as.character()),
    exclusive = exclusive, order = order
  )
}

check_ems_metals_data <- function(data, exclusive = FALSE, order = FALSE){
  check_data(
    data,
    values = list(
      SiteID = "character" ,
      COLLECTION_START = Sys.time(),
      COLLECTION_END = Sys.time(),
      REQUISITION_ID = 1,
      ANALYZING_AGENCY = "character",
      UPPER_DEPTH = units::as_units("m"),
      LOWER_DEPTH = units::as_units("m"),
      ReplicateID = as.integer(1),
      `Alkalinity Phen. 8.3` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Alkalinity Phen. 8.3` = units::as_units(c(NA, 1), "mg/l"),
      `Aluminum Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Aluminum Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Aluminum Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Aluminum Total` = units::as_units(c(NA, 1), "mg/l"),
      `Antimony Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Antimony Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Antimony Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Antimony Total` = units::as_units(c(NA, 1), "mg/l"),
      `Arsenic Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Arsenic Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Arsenic Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Arsenic Total` = units::as_units(c(NA, 1), "mg/l"),
      `Barium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Barium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Barium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Barium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Beryllium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Beryllium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Beryllium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Beryllium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Bicarbonate Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Bicarbonate Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Bismuth Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Bismuth Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Bismuth Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Bismuth Total` = units::as_units(c(NA, 1), "mg/l"),
      `Boron Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Boron Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Boron Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Boron Total` = units::as_units(c(NA, 1), "mg/l"),
      `Cadmium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Cadmium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Cadmium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Cadmium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Calcium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Calcium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Calcium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Calcium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Carbonate Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Carbonate Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Chromium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Chromium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Chromium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Chromium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Cobalt Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Cobalt Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Cobalt Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Cobalt Total` = units::as_units(c(NA, 1), "mg/l"),
      `Copper Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Copper Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Copper Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Copper Total` = units::as_units(c(NA, 1), "mg/l"),
      `Hardness (Dissolved)` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Hardness (Dissolved)` = units::as_units(c(NA, 1), "mg/l"),
      `Hardness Total (Total)` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Hardness Total (Total)` = units::as_units(c(NA, 1), "mg/l"),
      `Hydroxide Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Hydroxide Alkalinity` = units::as_units(c(NA, 1), "mg/l"),
      `Iron Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Iron Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Iron Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Iron Total` = units::as_units(c(NA, 1), "mg/l"),
      `Lead Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Lead Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Lead Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Lead Total` = units::as_units(c(NA, 1), "mg/l"),
      `Magnesium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Magnesium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Magnesium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Magnesium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Manganese Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Manganese Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Manganese Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Manganese Total` = units::as_units(c(NA, 1), "mg/l"),
      `Molybdenum Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Molybdenum Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Molybdenum Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Molybdenum Total` = units::as_units(c(NA, 1), "mg/l"),
      `Nickel Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nickel Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Nickel Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nickel Total` = units::as_units(c(NA, 1), "mg/l"),
      `Phosphorus Total Dissolved metals` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Phosphorus Total Dissolved metals` = units::as_units(c(NA, 1), "mg/l"),
      `Phosphorus Total metals` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Phosphorus Total metals` = units::as_units(c(NA, 1), "mg/l"),
      `Potassium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Potassium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Potassium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Potassium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Selenium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Selenium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Selenium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Selenium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Silicon Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Silicon Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Silicon Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Silicon Total` = units::as_units(c(NA, 1), "mg/l"),
      `Silver Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Silver Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Silver Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Silver Total` = units::as_units(c(NA, 1), "mg/l"),
      `Sodium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Sodium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Sodium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Sodium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Strontium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Strontium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Strontium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Strontium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Sulfur Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Sulfur Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Sulfur Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Sulfur Total` = units::as_units(c(NA, 1), "mg/l"),
      `Thallium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Thallium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Thallium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Thallium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Tin Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Tin Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Tin Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Tin Total` = units::as_units(c(NA, 1), "mg/l"),
      `Titanium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Titanium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Titanium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Titanium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Uranium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Uranium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Uranium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Uranium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Vanadium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Vanadium Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Vanadium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Vanadium Total` = units::as_units(c(NA, 1), "mg/l"),
      `Zinc Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Zinc Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Zinc Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Zinc Total` = units::as_units(c(NA, 1), "mg/l")),
      key = c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
                       "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH", "ReplicateID"),
    exclusive = exclusive, order = order
  )
}


check_ems_standard_data <- function(data, exclusive = FALSE, order = FALSE){
  check_data(
    data,
    values = list(
      SiteID = "character" ,
      COLLECTION_START = Sys.time(),
      COLLECTION_END = Sys.time(),
      REQUISITION_ID = 1,
      ANALYZING_AGENCY = "character",
      UPPER_DEPTH = units::as_units("m"),
      LOWER_DEPTH = units::as_units("m"),
      ReplicateID = as.integer(1),
      `Alkalinity Total 4.5` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Alkalinity Total 4.5` = units::as_units(c(NA, 1), "mg/l"),
      `Carbon Total Inorganic` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Carbon Total Inorganic` = units::as_units(c(NA, 1), "mg/l"),
      `Carbon Total Organic` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Carbon Total Organic` = units::as_units(c(NA, 1), "mg/l"),
      `Carbon Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Carbon Total` = units::as_units(c(NA, 1), "mg/l"),
      `Chlorophyll A` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Chlorophyll A` = units::as_units(c(NA, 1), "mg/l"),
      `Nitrate (NO3) Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nitrate (NO3) Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Nitrate(NO3) + Nitrite(NO2) Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nitrate(NO3) + Nitrite(NO2) Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Nitrogen - Nitrite Dissolved (NO2)` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nitrogen - Nitrite Dissolved (NO2)` = units::as_units(c(NA, 1), "mg/l"),
      `Nitrogen Ammonia Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nitrogen Ammonia Total` = units::as_units(c(NA, 1), "mg/l"),
      `Nitrogen Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Nitrogen Total` = units::as_units(c(NA, 1), "mg/l"),
      `Phosphorus Ort.Dis-P` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Phosphorus Ort.Dis-P` = units::as_units(c(NA, 1), "mg/l"),
      `Phosphorus Total Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Phosphorus Total Dissolved` = units::as_units(c(NA, 1), "mg/l"),
      `Phosphorus Total` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Phosphorus Total` = units::as_units(c(NA, 1), "mg/l"),
      `pH` = units::as_units(c(NA, 1), "pH"),
      `Limit pH` = units::as_units(c(NA, 1), "pH"),
      `Silica Reactive Diss` = units::as_units(c(NA, 1), "mg/l"),
      `Limit Silica Reactive Diss` = units::as_units(c(NA, 1), "mg/l"),
      `Turbidity` = units::as_units(c(NA, 1), "NTU"),
      `Limit Turbidity` = units::as_units(c(NA, 1), "NTU")),
      key = c("SiteID", "COLLECTION_START", "COLLECTION_END", "REQUISITION_ID",
          "ANALYZING_AGENCY", "UPPER_DEPTH", "LOWER_DEPTH", "ReplicateID"),
    exclusive = exclusive, order = order
  )
}

check_ems_raw_data <- function(data, exclusive = FALSE, order = FALSE){
  chk_data(data)
  check_names(
    data,
    names = c(
         "EMS_ID",
         "MONITORING_LOCATION",
         "LATITUDE",
         "LONGITUDE",
         "LOCATION_TYPE",
         "COLLECTION_START",
         "COLLECTION_END",
         "LOCATION_PURPOSE",
         "PERMIT",
         "PERMIT_RELATIONSHIP",
         "DISCHARGE_TO",
         "REQUISITION_ID",
         "SAMPLING_AGENCY",
         "ANALYZING_AGENCY",
         "COLLECTION_METHOD",
         "SAMPLE_CLASS",
         "SAMPLE_STATE",
         "SAMPLE_DESCRIPTOR",
         "PARAMETER_CODE",
         "PARAMETER",
         "ANALYTICAL_METHOD_CODE",
         "ANALYTICAL_METHOD",
         "RESULT_LETTER",
         "RESULT",
         "UNIT",
         "METHOD_DETECTION_LIMIT",
         "MDL_UNIT",
         "QA_INDEX_CODE",
         "UPPER_DEPTH",
         "LOWER_DEPTH",
         "TIDE",
         "AIR_FILTER_SIZE",
         "AIR_FLOW_VOLUME",
         "FLOW_UNIT",
         "COMPOSITE_ITEMS",
         "CONTINUOUS_AVERAGE",
         "CONTINUOUS_MAXIMUM",
         "CONTINUOUS_MINIMUM",
         "CONTINUOUS_UNIT_CODE",
         "CONTINUOUS_DURATION",
         "CONTINUOUS_DURATION_UNIT",
         "CONTINUOUS_DATA_POINTS",
         "TISSUE_TYPE",
         "SAMPLE_SPECIES",
         "SEX",
         "LIFE_STAGE",
         "BIO_SAMPLE_VOLUME",
         "VOLUME_UNIT",
         "BIO_SAMPLE_AREA",
         "AREA_UNIT",
         "BIO_SIZE_FROM",
         "BIO_SIZE_TO",
         "SIZE_UNIT",
         "BIO_SAMPLE_WEIGHT",
         "WEIGHT_UNIT",
         "BIO_SAMPLE_WEIGHT_FROM",
         "BIO_SAMPLE_WEIGHT_TO",
         "WEIGHT_UNIT_1",
         "SPECIES",
         "RESULT_LIFE_STAGE"),
    exclusive = exclusive, order = order
  )
  data
}

check_site_date_lookup <- function(data, exclusive = FALSE, order = FALSE) {
  check_data(
    data,
    values = list(
      File = "character",
      Date = "character",
      SiteID = "character"),
    key = c("File"),
    exclusive = exclusive, order = order
  )
}

check_mysid_raw_data <- function(data, exclusive = FALSE, order = FALSE){
  chk_data(data)
  check_names(
    data,
    names = c("FileName", "Date", "Year", "Month", "Day", "Station", "Replicate",
              "Time", "Depth", "SideLake", "#splitsCounted", "#splitsMade",
              "DenTotal", "Djuv", "DimmM", "DmatM", "DbreedM", "DimmF", "DmatF",
              "DbroodF", "DspentF", "DdistBrF", "BiomTotal", "Bjuv", "BimmM",
              "BmatM", "BbreedM", "BimmF", "BmatF", "BbroodF", "BspentF", "BdistBrF",
              "VolDenTotal", "VolDjuv", "VolDimmM", "VolDmatM", "VolDbreedM",
              "VolDimmF", "VolDmatF", "VolDbroodF", "VolDspentF", "VolDdisBrF",
              "Eggs/BroodF", "Eggs/DistBrF", "Eggs/Total#Mysids", "PropFemGravid"),
    exclusive = exclusive, order = order
  )
  invisible(data)
}

check_zoo_raw_data <- function(data, exclusive = FALSE, order = FALSE){
  chk_data(data)
  check_names(
    data,
    names = c("FileName", "YearComp", "Year", "Month", "Date1", "Date2",
              "Station", "Season", "Basin", "Replicate", "SexFecCode", "CopStageCode",
              "DenTotal", "DCopep", "DClad", "DClad other than Daph", "DDash",
              "DDkenai", "DEpi", "DCycl", "DNaup", "DDaph", "DDiaph", "DBosm",
              "DScap", "DLepto", "DCerio", "DChyd", "DOtherCopep", "DOtherClad",
              "DDashM", "DDashF", "DDash5", "DDash4", "DDash3", "DDash2", "DDash1",
              "DDashC", "DDkenaiM", "DDkenaiF", "DDkenaiC", "DEpiM", "DEpiF",
              "DEpiC", "DCyclM", "DCyclF", "DCycl5", "DCycl4", "DCycl3", "DCycl2",
              "DCycl1", "DCyclC", "BiomTotal", "BCopep", "BClad", "BClad other than Daph",
              "BDash", "BDkenai", "BEpi", "BCycl", "BNaup", "BDaph", "BDiaph",
              "BBosm", "BScap", "BLepto", "BCerio", "BChyd", "BOtherCopep",
              "BOtherClad", "BDashM", "BDashF", "BDash5", "BDash4", "BDash3",
              "BDash2", "BDash1", "BDashC", "BDkenaiM", "BDkenaiF", "BDkenaiC",
              "BEpiM", "BEpiF", "BEpiC", "BCyclM", "BCyclF", "BCycl5", "BCycl4",
              "BCycl3", "BCycl2", "BCycl1", "BCyclC", "F1Dash", "F1Dkenai",
              "F1Epi", "F1Cycl", "F1Daph", "F1Diaph", "F1Bosm", "F1Scap", "F1Lepto",
              "F1Cerio", "F1Chyd", "F2Dash", "F2Dkenai", "F2Epi", "F2Cycl",
              "F2Daph", "F2Diaph", "F2Bosm", "F2Scap", "F2Lepto", "F2Cerio",
              "F2Chyd", "F3Dash", "F3Dkenai", "F3Epi", "F3Cycl", "F3Daph",
              "F3Diaph", "F3Bosm", "F3Scap", "F3Lepto", "F3Cerio", "F3Chyd",
              "F4Dash", "F4Dkenai", "F4Epi", "F4Cycl", "F4Daph", "F4Diaph",
              "F4Bosm", "F4Scap", "F4Lepto", "F4Cerio", "F4Chyd", "F5Dash",
              "F5Dkenai", "F5Epi", "F5Cycl", "F5Daph", "F5Diaph", "F5Bosm",
              "F5Scap", "F5Lepto", "F5Cerio", "F5Chyd", "F6Dash", "F6Dkenai",
              "F6Epi", "F6Cycl", "F6Daph", "F6Diaph", "F6Bosm", "F6Scap", "F6Lepto",
              "F6Cerio", "F6Chyd", "HaulTime", "ENDREV", "STARTREV", "TOTREV",
              "SPLmade", "SPLcount", "vertical haul depth_m", "vertical haul net size_m",
              "Funding Source", "Field Collection", "Analyst"),
    exclusive = exclusive, order = order
  )
  invisible(data)
}

