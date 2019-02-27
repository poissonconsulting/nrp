check_path <- function(path) {
  check_string(path)
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
      SiteID = "character",
      DateTime = Sys.time(),
      Depth = units::as_units("m"),
      Temperature = units::as_units("degree * C"),
      Oxygen = units::as_units("mg/l"),
      Oxygen2 = units::as_units("percent"),
      Conductivity = units::as_units("uS/cm"),
      Conductivity2 = units::as_units("mu * S/cm"),
      Salinity = units::as_units("PSU"),
      Backscatter = units::as_units("NTU"),
      Fluorescence = units::as_units("ug/L"),
      Frequency = units::as_units(c(NA, 1), "Hz"),
      Flag = 1,
      Pressure = units::as_units(c(NA, 1), "dbar")),
      key = c("SiteID", "DateTime", "Depth"),
    exclusive = exclusive, order = order
  )
}
