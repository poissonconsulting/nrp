#' Create NRP SQLite Database
#'
#' @param path A string of the path to the database to create.
#' It must end with extension '.sqlite'.
#' @param ask A flag specifying whether to ask before deleting an existing file.
#' @return The path to the database.
#' @export
nrp_create_db <- function(path, ask = getOption("nrp.ask", TRUE)) {
  check_ext(path)
  check_flag(ask)

  if(file_exists(path)) {
    if(ask && !yesno("Really delete file '", path, "'?"))
      return(path)
    file_delete(path)
  }

  # create database here and initialize units
}
