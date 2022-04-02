#' @rdname natural.tolerance.poisson.simple
natural.tolerance.poisson <- function(
    x, 
    ...
) {
    m <- mean(x)

    natural.tolerance.poisson.simple(
        lambda = m,
        , ...
    )
}