ogive.ungrouped <- function(
  x
  ,col="blue"
  ,main="Ungrouped Ogive"
  ,ylab = "Cumulative Relative Frequency"
  ,xlab = "x"
  ,call.plot = T
  ,...
) {
  dist.ungrouped <- frequency.dist.ungrouped(x
                                             ,na.rm = T)
  
  x <- dist.ungrouped$value
  
  if (call.plot) {
    plot(x
         ,dist.ungrouped$cum.up
         , type="b"
         , main = main
         , ylab = ylab
         , xlab = xlab
         , col = col
         , ...
         )
  }
  
  invisible(list(x=x, y=dist.ungrouped$cum.up))
}