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
