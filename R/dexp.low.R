#' Shifted Exponential Distribution 
#' 
#' Calculations for shifted exponential distribution, sometimes called the exponential(low) distribution. 
#' This is equivalent to a regular exponential distribution, but shifted on the X axis left or right so that 
#' it has a non-zero origin. It can be specified in two ways, using a minimum and rate parameter or using 
#' a minimum and mean parameter.
#'
#' @param x Scalar - numeric - quantile.
#' @param q Scalar - numeric - quantile.
#' @param p Scalar - numeric - probability.
#' @param n Scalar - integer - number of observations.
#' @param low Scalar - numeric - Minimum distribution value.
#' @param rate Scalar - numeric - distribution rate.
#' @param mean Scalar - numeric - mean of distribution. If specified, rate is 1/(mean-low)
#' @param log Scalar - logical - if true, probabilities reported as log(p).
#' @param log.p Scalar - logical - if true, probabilities reported as log(p).
#' @param lower.tail Scalar - logical - if true, probabilities are lower tail probabilities.
#'
#' 
#' @aliases pexp.low qexp.low rexp.low
#'
#' @return dexp.low returns density function, pexp.low returns distribution function, qexp.low returns quantiles, and rexp.low returns random value(s).
#'
dexp.low <- function (
    x, 
    rate = 1, 
    low = 0, 
    log = FALSE
) {
    dexp(x = x - low, rate = rate, log = log)
}

#' @rdname dexp.low
pexp.low <- function(
    q, 
    rate = 1, 
    low = 0, 
    mean = NA,
    lower.tail = TRUE, 
    log.p = FALSE
) {
    if (is.na(mean)) {
        pexp(q = q-low, rate = rate, lower.tail = lower.tail, log.p = log.p)
    } else {
        pexp(q = q-low, rate = 1/(mean-low), lower.tail = lower.tail, log.p = log.p)
    }
}

#' @rdname dexp.low
qexp.low <- function(
    p, 
    rate = 1, 
    low = 0, 
    mean = NA,
    lower.tail = TRUE, 
    log.p = FALSE
) {
    if (is.na(mean)) {
        qexp(p = p, rate = rate, lower.tail = lower.tail, log.p = log.p) + low
    } else {
        qexp(p = p, rate = 1/(mean-low), lower.tail = lower.tail, log.p = log.p) + low
    }
    
}

#' @rdname dexp.low
rexp.low <- function(
    n, 
    rate = 1, 
    low = 0,
    mean = NA
) {
    if (is.na(mean)) {
        rexp(n, rate = rate) + low
    } else {
        rexp(n, rate = 1/(mean-low)) + low
    }
}

