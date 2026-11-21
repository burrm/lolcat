hist.add.distribution.curve.normal <- function(
    x,
    lwd = 3,
    col = "darkorange",
    freq = T,
    resolution = 1,
    ...
) {
    m <- mean(x)
    v <- var(x)

    hist.add.distribution.curve.normal.simple(
        mean = m, 
        variance = v, 
        lwd = lwd, 
        col = col, 
        plot.scale = if (!freq) { 1} else {length(x)*resolution},
        ...)
}