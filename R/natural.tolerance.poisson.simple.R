#' Natural Tolerance - Poisson Distribution 
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#' Note: for discrete distributions, tail areas may not exactly equal the input parameters.
#'
#' @param x Vector/numeric - data to fit parameters
#' @param lambda Scalar/numeric - rate parameter for normal distribution.
#' @param ... Additional parameters for natural.tolerance
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation. 
natural.tolerance.poisson.simple <- function(
    lambda = 5
    ,total.area = 1-.9973
    ,upper.tail = total.area / 2
    ,lower.tail = total.area / 2
    ,details = T
) {
    
    ret <- data.frame(natural.tolerance = numeric(0)
               ,lower.limit = numeric(0)
               ,upper.limit = numeric(0)
               ,lower.area = numeric(0)
               ,upper.area = numeric(0) 
               )

    for (i in 1:length(lambda)) {
        p.table <- table.dist.poisson(lambda = lambda[i])

        idx.lower   <- ifelse(any(p.table$eq.and.below <= lower.tail),max(which(p.table$eq.and.below <= lower.tail)),NA)
        lower.limit <- ifelse(!is.na(idx.lower), p.table$x[idx.lower],NA)
        lower.area  <- ifelse(!is.na(idx.lower), p.table$eq.and.below[idx.lower], NA)

        idx.upper   <- ifelse(any(p.table$eq.and.above <= lower.tail), min(which(p.table$eq.and.above <= lower.tail)), NA)
        upper.limit <- ifelse(!is.na(idx.upper),p.table$x[idx.upper],NA)
        upper.area  <- ifelse(!is.na(idx.upper),p.table$eq.and.above[idx.upper],NA)

        nt <- upper.limit - lower.limit

        ret <- rbind(ret,
            data.frame(
                natural.tolerance = nt
               ,lower.limit = lower.limit
               ,upper.limit = upper.limit
               ,lower.area = lower.area
               ,upper.area = upper.area 
               )
        )

    }

  if (!details) {
    ret$natural.tolerance
  } else {
    ret
  }

    
}