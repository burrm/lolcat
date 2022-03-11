#' SPC Run Chart 
#'
#' Provides a simple control chart a series without calculating limits  
#'
#' @param chart.series Vector - Chart series
#' @param type Character - Chart display type, default is line and point
#' @param main Character - Main chart title
#' @param xlab Character - X axis title
#' @param ylab Character - Y axis title
#' @param col Character - Color of lines/points
#' @param pch Integer - Point Display Character
#' @param ... Additional parameters
#'
#' @aliases spc.run.chart
#'
#' @return Invisible return of plot(...). 
spc.chart.run<-function(
  chart.series
  ,type="b"
  ,main = "Run Chart"
  ,xlab = "Time"
  ,ylab = "Criterion Measure"
  ,col= "blue"
  ,pch=19
  , ...) {
  plot(1:length(chart.series), type=type, chart.series, main=main, xlab = xlab, ylab = ylab, col=col, pch=pch,  ...)
}

#' @rdname spc.chart.run
spc.run.chart <- spc.chart.run