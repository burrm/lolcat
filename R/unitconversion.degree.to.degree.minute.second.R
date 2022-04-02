#' Unit Conversion - Angles - Degrees to Degrees, Minutes, and Seconds  
#' 
#' Performs a conversion of angle measures from degrees to degrees, minutes, and seconds. 
#' Minutes (also called minutes of arc) are 1/60th of a degree.
#' Seconds (also called seconds of arc) are 1/60th of a minute or 1/3600 of a degree.
#' Any remaining decimal will be included in the seconds portion of the resulting data structure. 
#' To avoid some rounding oddities due to floating point math, use round.object().
#' 
#'
#' @param x Vector - Values in units of degrees
#'
#' @return if x is length 1, then a single list, otherwise a nested list for each value of x, 
#' names for degrees, minutes, seconds. Sign of resulting values matches sign of input.  
#' 
#' @examples
#' ## Example 1 - positive and negative (3,3',5'')
#' #unitconversion.degree.to.degree.minute.second(
#' #  c(
#' #    -(3+(3/60)+(5/3600)),
#' #      3+(3/60)+(5/3600)
#' #  )
#' #)
#' 
#' ## Example 2 - convert multiple values to a data frame
#' #as.data.frame(
#' #  do.call(
#' #    rbind, 
#' #    unitconversion.degree.to.degree.minute.second(
#' #      c(
#' #        -(3+(3/60)+(5/3600)),
#' #          3+(3/60)+(5/3600)
#' #      )
#' #    )
#' #  )
#' #)


unitconversion.degree.to.degree.minute.second <- function(
  x = 1
) {
  ret <- list()
  for (i in 1:length(x)) {
    val <- x[i]
    aval <- abs(val)
    degrees <- floor(aval)

    rem1 <- 60*(aval-degrees)
    minutes <- floor(rem1)

    rem2 <- (rem1-minutes)
    seconds <- 60*rem2

    ret[[i]] <- list(
      degrees = sign(val)*degrees,
      minutes = sign(val)*minutes,
      seconds = sign(val)*seconds
    )
  }

  if (length(x) == 1) {
    ret <- ret[[1]]
  }

  ret
}