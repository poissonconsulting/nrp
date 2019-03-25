#' Upload CTD data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write_sqlite
#' @export
#'

# conn <- nrp_create_db(path  = "test", ask = FALSE)
# path <-  system.file("extdata", "ctd/2018",
#                      package = "nrp", mustWork = TRUE)
# data <- nrp_read_ctd(path = path, db_path = conn, recursive = TRUE)
# db_path = conn
# nrp_upload_ctd(data = data, db_path = db_path)

nrp_upload_ctd <- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_close_connection(conn = conn))
  }

  visit <- group_by(data, .data$SiteID, .data$Date, .data$Time) %>%
    summarise(DepthDuplicates = length(which(.data$Retain == FALSE)), File = first(.data$File)) %>%
    ungroup()


  visit_db <- nrp_download_ctd_visit(db_path = conn)
  visit_upload <- setdiff(visit, visit_db)

  readwritesqlite::rws_write_sqlite(x = visit_upload, commit = commit, strict = strict, silent = silent,
                                    x_name = "visitCTD", conn = conn)

  n_pre_filt <- nrow(data)
  data %<>% filter(.data$Retain == TRUE)
  n_dups <- n_pre_filt - nrow(data)
  message(paste(n_dups, "duplicate depths removed from data"))

  data %<>% select(-.data$File)

  readwritesqlite::rws_write_sqlite(x = data, commit = commit, strict = strict, silent = silent,
                                    x_name = "CTD", conn = conn)
}

