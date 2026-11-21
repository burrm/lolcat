#' Extract Value By Name  
#' 
#' Extracts a value based on name from a named vector. 
#'
#' @param obj Vector - values
#' @param name Character - name of value to extract
#'
#' @return x, but converted to meters 
utility.extract.named.value <- function(
  obj = c(a= 1, b = 2, c= 3), #named atomic
  name = "b"                  #name
) {
  idx <- which(names(obj) == name)
  rmnames(obj[idx])
}