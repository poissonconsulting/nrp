check_file_exists <- function(path) {
  check_string(path)
  if(!file_exists(path)) err("path '", path, "' must exist")
  if(!is_file(path)) err("path '", path, "' must be a file")
  path
}

check_dir_exists <- function(path) {
  check_string(path)
  if(!file_exists(path)) err("path '", path, "' must exist")
  if(!is_dir(path)) err("path '", path, "' must be a directory")
  path
}

check_ctd_data <- function(data, exclusive = FALSE, order = FALSE) {
  check_data(
    data,
    values = list(
      DateTime = Sys.time(),
      Depth = 1,
      Temperature = 1,
      Oxygen = 1,
      Oxygen2 = 1,
      Conductivity = 1,
      Conductivity2 = 1,
      Salinity = 1,
      Backscatter = 1,
      Fluorescence = 1,
      Frequency = 1,
      Flag = 1,
      Pressure = 1),
    exclusive = exclusive, order = order
  )
}