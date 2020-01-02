confusion.matrix.fn.multiple.values <- function(values) {
  function(x) {
    ifelse(x %in% value,1,0)
  }
}