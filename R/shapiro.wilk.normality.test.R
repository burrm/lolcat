#' Shapiro-Wilk Normality Test  
#' 
#' Calculate Shapiro-Wilk normality test for a variable. 
#' Low p-values indicate rejection of assumption of normality.
#'
#' @param x A vector of values
#'
#' @return An htest object with results of test.  

shapiro.wilk.normality.test <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)

  rng <- x[n] - x[1L]
  
  if (!is.na(n) && n >= 3L && n <= 5000L && rng != 0) {
    ret <- shapiro.test(x)
  } else {
    ret <- list(
      statistic = c(W = NA)
      ,p.value = NA
      ,method = "Shapiro-Wilk Normality Test"
      ,data.name = DNAME
    )
    class(ret) <- "htest"
  }

  return(ret)
}
