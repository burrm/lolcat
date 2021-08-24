natural.tolerance.chisquare.simple <- function(
    df = 1, 
    ncp = 0, 
    ...
) {
    natural.tolerance(
        function(p, lower.tail) {
            qchisq(
                p = p, 
                df = df,
                ncp = ncp,
                lower.tail = lower.tail
            )
        }, ...
    )
}