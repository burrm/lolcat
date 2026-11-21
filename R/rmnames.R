#' Remove Names From Object  
#' 
#' Returns the input object, but without names. 
#'
#' @param x Vector - The object to remove names from
#'
#' @return x, but without names 
rmnames<-function(x) {
  ret <- x
  names(ret)<-NULL
  ret
}