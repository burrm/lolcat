natural.tolerance.exp.low <- function(
    x, 
    low = NA,
    ...
) {
    if (is.na(low)) {
        low <- min(x)
    }

    rate <- 1/mean(x - low)

    natural.tolerance.exp.low.simple(
        rate = rate,
        low = low,
        , ...
    )
}