confusion.matrix.fn.value <- function(value) {
  function(x) {
    ifelse(x == value,1,0)
  }
}