spc.center.line.median.moving.range <- function(x) {
  median(abs(diff(x)))
}