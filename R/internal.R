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

clean_input_cols <- function(data, lookup){

  missing_cols <- names(lookup)[!names(lookup) %in% names(data)]
  excess_cols <- names(data)[!names(data) %in% names(lookup)]

  if(length(missing_cols) > 0) {
    warning <- paste("The input data is missing the following columns which will be assigned NA: ",
                     paste(missing_cols, collapse = ", "))
    warning(warning)
    data[missing_cols] <- NA
  }

  if(length(excess_cols) > 0) {
    warning <- paste("The input data contains the following unrecognized columns that will be discarded: ",
                     paste(excess_cols, collapse = ", "))
    warning(warning)
    data[excess_cols] <- NULL
  }

  reclassed <- map_dfc(names(data), function(x){
    col <- dplyr::pull(data, x)
    new_class <- as.character(lookup[x])

    if(new_class == "date") {
      col <- dttr2::dtt_date(as.integer(col), origin = "1899-12-30")
    } else {
      col <- suppressWarnings(methods::as(col, new_class))
    }

    col <- as.data.frame(col)
    names(col) <- x
    as_tibble(col)
  })

  select(reclassed, all_of(names(lookup)))

}
