named_list <- function()
  list(x = 1)[-1]

all_na <-function(data){
  apply(data, 1, function(x){all(is.na(x))})
}
