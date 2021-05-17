named_list <- function()
  list(x = 1)[-1]

all_na <-function(data){
  apply(data, 1, function(x){all(is.na(x))})
}

fill_date_query <- function(table, col, end, start, connection) {

  if(is.null(start) & is.null(end)){
    end <- DBI::dbGetQuery(connection, paste0("SELECT MAX(`", col, "`) FROM ", table))[1,1]
    start <- as.character(dttr2::dtt_add_years(dttr2::dtt_date(end), -1))
  } else if (is.null(start) & !is.null(end)){
    start <- DBI::dbGetQuery(connection, paste0("SELECT MIN(`", col, "`) FROM ", table))[1,1]
  } else if (!is.null(start) & is.null(end)){
    end <- DBI::dbGetQuery(connection, paste0("SELECT MAX(`", col, "`) FROM ", table))[1,1]
  }
  c("start_date" = start, "end_date" = end)
}

fill_date_time_query <- function(table, col, end, start, connection) {

  if(is.null(start) & is.null(end)){
    end <- DBI::dbGetQuery(connection, paste0("SELECT MAX(`", col, "`) FROM ", table))[1,1]
    start <- as.character(dttr2::dtt_add_years(dttr2::dtt_date_time(end), -1))
  } else if (is.null(start) & !is.null(end)){
    start <- DBI::dbGetQuery(connection, paste0("SELECT MIN(`", col, "`) FROM ", table))[1,1]
  } else if (!is.null(start) & is.null(end)){
    end <- DBI::dbGetQuery(connection, paste0("SELECT MAX(`", col, "`) FROM ", table))[1,1]
  }
  c("start_date_time" = start, "end_date_time" = end)
}
