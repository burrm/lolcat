natural.tolerance.normal.simple <- function(
    mean = 1, 
    variance = 1, 
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qnorm(
                p = p, 
                mean = mean,
                sd = sqrt(variance),
                lower.tail = lower.tail
            )
        }, ...
    )
}