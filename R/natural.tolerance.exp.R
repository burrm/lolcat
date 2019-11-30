natural.tolerance.exp <- function(
    x, 
    ...
) {
    rate <- 1/mean(x)
   
    natural.tolerance.exp.simple(
        rate = rate,
        , ...
    )
}