fill_units <- function(x, value){

  if(inherits(x, "units") || !inherits(x, "numeric") || is.na(value)){
    result <- x
  } else {
    result <- units::set_units(x, value, mode = "standard")
  }
  result
}


extract_units <- function(units_list){
  meta_units <- c()
  for(i in 1:length(units_list)){
    if(length(as.character(units_list[[i]]$unit)) == 0){
      meta_units %<>% append(NA)
    } else {
      meta_units %<>% append(as.character(units_list[[i]]$unit))
    }
  }
  meta_units
}
