connect_if_valid_path <- function(path){
  check_file_exists(path)
  suppressWarnings(withCallingHandlers(
    conn <- readwritesqlite::rws_open_connection(dbname = path,  exists = TRUE),
    warning=function(w) {
      if (str_detect(w$message, "Couldn't set synchronous mode: file is not a database"))
        err("File provided is not an SQLite database")
    }))
  conn
}
