#' Upload CTD data to nrp database
#'
#' @param data the object name of the data to be uploaded
#' @param db_path An Sqlite Database Connection, or path to an SQLite Database
#' @inheritParams readwritesqlite::rws_write
#' @export
#'

nrp_upload_ctd <- function(data, db_path = getOption("nrp.db_path", NULL), commit = TRUE, strict = TRUE, silent = TRUE){
  conn <- db_path
  if(!inherits(conn, "SQLiteConnection")){
    conn <- connect_if_valid_path(path = conn)
    on.exit(readwritesqlite::rws_disconnect(conn = conn))
  }

  visit <- group_by(data, .data$SiteID, .data$Date, .data$Time) %>%
    summarise(DepthDuplicates = length(which(.data$Retain == FALSE)), File = first(.data$File)) %>%
    ungroup()


  visit_db <- nrp_download_ctd_visit(db_path = conn)
  visit_upload <- setdiff(visit, visit_db)

  readwritesqlite::rws_write(x = visit_upload, commit = commit, strict = strict, silent = silent,
                                    x_name = "visitCTD", conn = conn)

  n_pre_filt <- nrow(data)
  data %<>% filter(.data$Retain == TRUE)
  n_dups <- n_pre_filt - nrow(data)
  message(paste(n_dups, "duplicate depths removed from data"))

  data %<>% select(-.data$File)

  readwritesqlite::rws_write(x = data, commit = commit, strict = strict, silent = silent,
                                    x_name = "CTD", conn = conn)
}
