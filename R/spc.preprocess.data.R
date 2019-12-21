spc.preprocess.data <- function(data, sample, ...) {
    #calculate group statistics
    d <- data.frame(dv=data, g=sample)
    out <- summary.impl(dv~g, data=d, ...)

    #reorder based on input frame
    l <- rle(sample)$values
    o <- sapply(l, FUN=function(i) { which(i == out$g)})

    out <- out[o,]
    rownames(out) <- 1:nrow(out)

    out
}