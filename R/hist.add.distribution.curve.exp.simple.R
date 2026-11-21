hist.add.distribution.curve.exp.simple <- function (
    rate = 1, 
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
            plot.scale*dexp(x, rate = rate) 
        }, 
        lim[1], 
        lim[2], 
        add=TRUE, 
        lwd = lwd,
        col = col,
        ...
    )
}