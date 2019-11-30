natural.tolerance.normal <- function(
    x, 
    ...
) {
    m <- mean(x)
    v <- var(x)

    natural.tolerance.normal.simple(
        mean = m,
        variance = v,
        , ...
    )
}