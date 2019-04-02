#' Initialize ctd table for database
#'
#' @return initialized table with units
initialize_ctd <- function(){
  ctd <- tibble(FileID = integer(),
                SiteID = character(),
                Date = as.Date(character()),
                Time = dttr::dtt_time(character()),
                Depth = units::set_units(numeric(), "m"),
                Temperature = units::set_units(numeric(), "degC"),
                Oxygen = units::set_units(numeric(), "mg/l"),
                Oxygen2 = units::set_units(numeric(), "percent"),
                Conductivity = units::set_units(numeric(), "uS/cm"),
                Conductivity2 = units::set_units(numeric(), "mu * S/cm"),
                Salinity = units::set_units(numeric(), "PSU"),
                Backscatter = units::set_units(numeric(), "NTU"),
                Fluorescence = units::set_units(numeric(), "ug/L"),
                Frequency = units::set_units(numeric(), "Hz"),
                Flag = numeric(),
                Pressure = units::set_units(numeric(), "dbar"))
  ctd
}

initialize_ctd_visit <- function(){
  visit <- tibble(SiteID = character(),
           Date = as.Date(character()),
           Time = dttr::dtt_time(character()),
           DepthDuplicates = integer(),
           File = character())
}

