spc.run.chart<-function(x, type="b", main = "Individuals Chart", xlab = "Time", ylab = "Criterion Measure", col= "blue", pch=19, ...) {
  plot(1:length(x), type=type, x, main=main, xlab = xlab, ylab = ylab, col=col, pch=pch,  ...)
}
