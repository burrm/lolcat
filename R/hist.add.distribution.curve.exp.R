hist.add.distribution.curve.exp <- function(
    x,
    lwd = 3,
    col = "darkorange",
    freq = T,
    resolution = 1,
    ...
) {
    rate <- 1/mean(x)

    hist.add.distribution.curve.exp.simple(
        rate = rate, 
        lwd = lwd, 
        col = col, 
        plot.scale = if (!freq) { 1} else {length(x) * resolution},
        ...)
}