#' Round Numeric Values in Object  
#' 
#' Returns the input object, but rounded. 
#'
#' @param x Vector - The object to remove names from
#' @param digits Scalar - The number of digits to round to
#'
#' @return x, but rounded 
round.object <- function(
  x,
  digits = 4
) {
  attr.x <- attributes(x)
  
  if (is.atomic(x) && is.numeric(x) && !is.factor(x)) {
    x <- round(x, digits)
  } else if (is.data.frame(x)) {
    x <- as.data.frame(round.object(as.list(x), digits = digits))
  } else if (is.list(x)) {
    x <- lapply(x, FUN = function(z) {
      round.object(z, digits = digits)
    })
  }
  
  attributes(x) <- attr.x
    
  x
}