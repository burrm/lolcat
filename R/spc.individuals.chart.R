# spc.individuals.chart<-function(x 
#                                 ,set = rep(1,length(x))
#                                 ,xchart.center.line.fn = function(x) {mean(x)}
#                                 ,xchart.dispersion.fn = function(x) {sqrt(var(x))}
#                                 
#                                 ,rchart.center.line.fn = function(d) { mean(d)}
#                                 
#                                                                 
#                                 ,info.xchart     = T
#                                 ,display.xchart  = T
#                                 ,xchart.main     = "Individuals Chart" 
#                                 ,xchart.xlab     = "Time"
#                                 ,xchart.ylab     = "Criterion Measure" 
#                                 ,xchart.col      = c("blue", "orange") 
#                                 ,xchart.line.col = "grey"
#                                 ,xchart.pch      = c(19,19)
#                                 
#                                 ,info.rchart     = T
#                                 ,display.rchart  = T
#                                 ,rchart.main     = "Moving Range Chart" 
#                                 ,rchart.xlab     = "Time"
#                                 ,rchart.ylab     = "Moving Range" 
#                                 ,rchart.col      = c("blue", "orange") 
#                                 ,rchart.line.col = "grey"
#                                 ,rchart.pch      = c(19,19)
#                                 
#                                 ,...) {
#   plot(1:length(x), x, main=main, xlab = xlab, ylab = ylab, col=col, pch=pch,  ...)
# }
