#' Unit Conversion - Angles - Degrees to Degrees and Minutes  
#' 
#' Performs a conversion of angle measures from degrees to degrees and minutes. 
#' Minutes (also called minutes of arc) are 1/60th of a degree.
#' Any remaining decimal will be included in the minutes portion of the resulting data structure. 
#' 
#'
#' @param x Vector - Values in units of degrees
#'
#' @return if x is length 1, then a single list, otherwise a nested list for each value of x, names for 
#' degrees and minutes. Sign of resulting values matches sign of input. 
unitconversion.degree.to.degree.minute <- function(
  x = 1
) {
  ret <- list()
  for (i in 1:length(x)) {
    val <- x[i]
    aval <- abs(val)
    degrees <- floor(aval)

    rem1 <- 60*(aval-degrees)
    minutes <- rem1


    ret[[i]] <- list(
      degrees = sign(val)*degrees,
      minutes = sign(val)*minutes
    )
  }

  if (length(x) == 1) {
    ret <- ret[[1]]
  }

  ret
}