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
    lower.tail = TRUE, 
    log.p = FALSE
) {
    pexp(q = q-low, rate = rate, lower.tail = lower.tail, log.p = log.p)
}

qexp.low <- function(
    p, 
    rate = 1, 
    low = 0, 
    lower.tail = TRUE, 
    log.p = FALSE
) {
    qexp(p = p, rate = rate, lower.tail = lower.tail, log.p = log.p) + low
}

rexp.low <- function(
    n, 
    rate = 1, 
    low = 0
) {
    rexp(n, rate = rate) + low
}

