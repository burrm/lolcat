confusion.matrix.fn.threshold <- function(threshold) {
  f <- function(x) {
    ifelse(x > threshold,1,0)
  }
  f
}