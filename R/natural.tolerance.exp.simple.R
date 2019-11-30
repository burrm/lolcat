natural.tolerance.exp.simple <- function(
    rate = 1, 
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qexp(
                p = p, 
                rate = rate,
                lower.tail = lower.tail
            )
        }, ...
    )
}