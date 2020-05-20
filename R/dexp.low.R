dexp.low <- function (
    x, 
    rate = 1, 
    low = 0, 
    log = FALSE
) {
    dexp(x = x - low, rate = rate, log = log)
}

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

