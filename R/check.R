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

assess_data_type <- function(data){
  check_data_functions <- list("CTD" = purrr::safely(check_ctd_data))
  errors <- c()
  for(i in 1:length(check_data_functions)){
    if(is.null(check_data_functions[[i]](data)$error)){
      errors[i] <- "match"
    } else {
      errors[i] <- "non match"
    }
  }
  data_type_match <- names(check_data_functions)[errors == "match"]
  if(length(data_type_match) == 1){
    data_type_match
  } else if(length(data_type_match) > 1){
    err("The data you are attempting to upload matches more than one NRP data type")
  } else {
    err("The data you are attempting to upload does not match any known NRP data types")
  }
}
