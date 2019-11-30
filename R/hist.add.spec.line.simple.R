hist.add.spec.line.simple <- function(
    at = 0,
    at.y = NA,
    label = "TGT",
    lwd = 3,
    col = "chocolate4",
    ...
) {

    abline(v = at, lwd = lwd, col = col, ...)

    if (!is.na(label)) {
        if ( is.na(at.y)) {
            lim <- par("usr")
            at.y <- lim[3] + .66*(lim[4]-lim[3]) 
        }

        text(x=at,y=at.y,labels=label)
    }

}