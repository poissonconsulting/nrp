assess_data_type <- function(data){
  check_data_functions <- list("CTD" = purrr::safely(check_ctd_data))
  errors <- c()
  for(i in 1:length(check_data_functions)){
    if(is.null(check_data_functions[[i]](data)$error)){
      errors[i] <- "match"
    } else {
      errors[i] <- "non match"
    }
  }
  data_type_match <- names(check_data_functions)[errors == "match"]
  if(length(data_type_match) == 1){
    data_type_match
  } else if(length(data_type_match) > 1){
    err("The data you are attempting to upload matches more than one NRP data type")
  } else {
    err("The data you are attempting to upload does not match any known NRP data types")
  }
}
