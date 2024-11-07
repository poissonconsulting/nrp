fill_units <- function(x, value) {
  if(inherits(x, "units") || !inherits(x, "numeric") || is.na(value))
    return(x)
  units::set_units(x, value, mode = "standard")
}


extract_units <- function(units_list){
  meta_units <- c()
  for(i in seq_along(units_list)){
    if(length(as.character(units_list[[i]]$unit)) == 0){
      meta_units %<>% append(NA)
    } else {
      meta_units %<>% append(as.character(units_list[[i]]$unit))
    }
  }
  meta_units
}


pull_ems_units <- function(x){
  columns <- names(x)
  units <- c()
  for(i in seq_along(columns)){
    unit <- str_extract(columns[i], 'unit:.*')
    units %<>% append(unit)
  }
  units %<>% gsub(pattern = "unit:", replacement = "")
  units
}
