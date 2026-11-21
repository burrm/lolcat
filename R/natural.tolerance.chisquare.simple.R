#' Natural Tolerance - Chi Square Distribution 
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#'
#' @param df Scalar/numeric - degrees of freedom.
#' @param ncp Scalar/numeric - non-centrality parameter in case of noncentral chi square distribution.
#' @param ... Additional parameters for natural.tolerance
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation. 
natural.tolerance.chisquare.simple <- function(
    df = 1, 
    ncp = 0, 
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qchisq(
                p = p, 
                df = df,
                ncp = ncp,
                lower.tail = lower.tail
            )
        }, ...
    )
}