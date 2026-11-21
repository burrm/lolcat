#' True Mode  
#' 
#' Calculates the "true mode" for the data, defined as \eqn{3\tilde{x} - 2\bar{x}}{3*median(x)-2*mean(x)} 
#'
#' @param x Vector - data to calculate "true mode" for
#' 
#' @return Scalar result of true mode calculation. 
true.mode<-function(x) {
  3*median(x)-2*mean(x)
}