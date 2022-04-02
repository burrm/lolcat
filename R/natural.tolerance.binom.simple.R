#' Natural Tolerance - Binomial Distribution 
#' 
#' Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
#' F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
#' with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
#' Note: for discrete distributions, tail areas may not exactly equal the input parameters.
#'
#' @param size Scalar/numeric - Sample size
#' @param prob Scalar/numeric - probability of "success".
#' @param total.area Scalar/numeric - the total area to use for the calculation, default is .0027
#' @param upper.tail Scalar/numeric - upper tail area to use for the calculation, default is .00135
#' @param lower.tail Scalar/numeric - lower tail area to use for the calculation, default is .00135
#' @param details Scalar/logical - Include calculation details, default is to return details
#' 
#' @return Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation.
natural.tolerance.binom.simple <- function(
    size = 10
    ,prob = .5 
    ,total.area = 1-.9973
    ,upper.tail = total.area / 2
    ,lower.tail = total.area / 2
    ,details = T
) {
    if (length(size) < length(prob)) {
        size <- rep_len(length(prob))
    }

    if (length(prob) < length(size)) {
        prob <- rep_len(length(size))
    }

    ret <- data.frame(natural.tolerance = numeric(0)
               ,lower.limit = numeric(0)
               ,upper.limit = numeric(0)
               ,lower.area = numeric(0)
               ,upper.area = numeric(0) 
               )

    for (i in 1:length(size)) {
        b.table <- table.dist.binomial(n = size[i], p=prob[i])

        idx.lower   <- ifelse(any(b.table$eq.and.below <= lower.tail),max(which(b.table$eq.and.below <= lower.tail)),NA)
        lower.limit <- ifelse(!is.na(idx.lower), b.table$x[idx.lower],NA)
        lower.area  <- ifelse(!is.na(idx.lower), b.table$eq.and.below[idx.lower], NA)

        idx.upper   <- ifelse(any(b.table$eq.and.above <= lower.tail), min(which(b.table$eq.and.above <= lower.tail)), NA)
        upper.limit <- ifelse(!is.na(idx.upper),b.table$x[idx.upper],NA)
        upper.area  <- ifelse(!is.na(idx.upper),b.table$eq.and.above[idx.upper],NA)

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