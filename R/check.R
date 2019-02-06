check_file_exists <- function(path) {
  check_string(path)
  if(!file_exists(path)) err("file '", path, "' must exist")
  if(!is_file(path)) err("file '", path, "' must be a file")
  path
}
