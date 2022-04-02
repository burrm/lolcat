#' Validation - alternative Parameter for Hypothesis Tests  
#' 
#' Verifies that the alternative hypothesis argument matches a set of correct values.
#' If nonconforming, then stop() is called
#'
#' @param alternative Scalar/character - The value to test
#' @param allowed Vector/character - Possible values
#'
#' @return no return value 
validate.htest.alternative <- function(
  alternative, 
  allowed = c("two.sided", "less", "greater")
) {
  if (length(alternative) < 1) {
    stop("Empty alternative parameter")
  }
  
  if (length(alternative) > 1) {
    alternative <- alternative[1]
  }

  if (is.na(alternative)) {
    stop("alternative parameter is NA")
  }
  
  if (!is.character(alternative)) {
    stop("alternative parameter is not a character string")
  }

  if (!(alternative %in% allowed)) {
    stop(paste("alternative parameter is invalid. Use", paste(shQuote(allowed, type="cmd"), collapse = ", ") ))
  }
}