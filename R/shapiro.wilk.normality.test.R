shapiro.wilk.normality.test <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)

  rng <- x[n] - x[1L]
  
  if (!is.na(n) && n >= 3L && n <= 5000L && rng != 0) {
    ret <- shapiro.test(x)
  } else {
    ret <- list(statistic = c(W = NA), p.value = NA, 
                method = "Shapiro-Wilk normality test", data.name = DNAME)
    class(ret) <- "htest"
  }

  return(ret)
}
