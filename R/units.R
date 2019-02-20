fill_units <- function(x, value){

  if(inherits(x, "units") || !inherits(x, "numeric")){
    result <- x
  } else {
    result <- units::set_units(x, value, mode = "standard")
  }
  result
}
