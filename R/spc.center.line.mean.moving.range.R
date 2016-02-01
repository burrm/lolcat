spc.center.line.mean.moving.range <- function(x) {
  mean(abs(diff(x)))
}