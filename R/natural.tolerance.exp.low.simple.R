#' Natural Tolerance - Exponential(low) Distribution 
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#'
#' @param x Vector/numeric - data to fit parameters
#' @param rate Scalar/numeric - rate parameter for exponential distribution.
#' @param low Scalar/numeric - minimum value for distribution
#' @param ... Additional parameters for natural.tolerance
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation. 
natural.tolerance.exp.low.simple <- function(
    rate = 1, 
    low = 0,
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qexp.low(
                p = p, 
                rate = rate,
                low = low,
                lower.tail = lower.tail
            )
        }, ...
    )
}