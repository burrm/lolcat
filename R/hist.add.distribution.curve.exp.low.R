hist.add.distribution.curve.exp.low <- function(
    x,
    low = NA,
    lwd = 3,
    col = "darkorange",
    freq = T,
    resolution = 1,
    ...
) {
    if (is.na(low)) {
        low <- min(x)
    }

    rate <- 1/mean(x - low)

    hist.add.distribution.curve.exp.low.simple(
        rate = rate, 
        low = low, 
        lwd = lwd, 
        col = col, 
        plot.scale = if (!freq) { 1} else {length(x)*resolution},
        ...)
}
