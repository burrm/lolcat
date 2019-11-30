natural.tolerance.exp.low.simple <- function(
    rate = 1, 
    low = 0,
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qexp.low(
                p = p, 
                rate = rate,
                low = low,
                lower.tail = lower.tail
            )
        }, ...
    )
}