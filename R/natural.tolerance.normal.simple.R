#' Natural Tolerance - Normal/Gaussian Distribution 
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#'
#' @param x Vector/numeric - data to fit parameters
#' @param mean Scalar/numeric - mean parameter for normal distribution.
#' @param variance Scalar/numeric - variance parameter for normal distribution, expressed as variance.
#' @param ... Additional parameters for natural.tolerance
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation. 
natural.tolerance.normal.simple <- function(
    mean = 1, 
    variance = 1, 
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qnorm(
                p = p, 
                mean = mean,
                sd = sqrt(variance),
                lower.tail = lower.tail
            )
        }, ...
    )
}