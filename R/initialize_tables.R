#' Initialize ctd table for database
#'
#' @return initialized table with units
initialize_ctd <- function(){
  ctd <- tibble(SiteID = character(), DateTime = as.POSIXct(character(), tz = "Etc/GMT+8"),
                Depth = units::set_units(numeric(), "m"), Temperature = units::set_units(numeric(), "degree * C"),
                Oxygen = units::set_units(numeric(), "mg/l"), Oxygen2 = units::set_units(numeric(), "%"),
                Conductivity = units::set_units(numeric(), "uS/cm"),
                Conductivity2 = units::set_units(numeric(), "mu * S/cm"), Salinity = numeric(),
                Backscatter = units::set_units(numeric(), "NTU"), Fluorescence = numeric(),
                Frequency = units::set_units(numeric(), "Hz"), Flag = as.logical(),
                Pressure = units::set_units(numeric(), "dbar"))
  ctd
}