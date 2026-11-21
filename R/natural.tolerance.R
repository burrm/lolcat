#' Natural Tolerance  
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#'
#' @param f Function - f(p, lower.tail) is a quantile function for the probability distribution in question. For example, qnorm.
#' @param total.area Scalar/numeric - the total area to use for the calculation, default is .0027
#' @param upper.tail Scalar/numeric - upper tail area to use for the calculation, default is .00135
#' @param lower.tail Scalar/numeric - lower tail area to use for the calculation, default is .00135
#' @param details Scalar/logical - Include calculation details, default is to return details
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation. 
natural.tolerance <- function(f #quantile function/closure
                                #interface is f(p, lower.tail)
                              ,total.area = 1-.9973
                              ,upper.tail = total.area / 2
                              ,lower.tail = total.area / 2
                              ,details = T
) {
  upper.limit <- f(p = upper.tail, lower.tail = F)
  lower.limit <- f(p = lower.tail, lower.tail = T)
  
  natural.tolerance = upper.limit - lower.limit
  
  if (!details) {
    natural.tolerance
  } else {
    data.frame(
      natural.tolerance = natural.tolerance
      ,lower.limit = lower.limit
      ,upper.limit = upper.limit
      ,lower.area = lower.tail
      ,upper.area = upper.tail 
     )
  }
  
}