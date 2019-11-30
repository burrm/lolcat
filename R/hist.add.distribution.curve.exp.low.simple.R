hist.add.distribution.curve.exp.low.simple <- function (
    rate = 1, 
    low = 0,
    lwd = 3,
    col = "darkorange",
    plot.scale = 1,
    ...
) {
    lim <- par("usr")
    #xlim <- lim[1:2]
    #ylim <- lim[3:4]
    
    plot( 
        function(x) { 
            plot.scale*dexp.low(x, rate = rate, low = low) 
        }, 
        lim[1], 
        lim[2], 
        add=TRUE, 
        lwd = lwd,
        col = col,
        ...
    )
}