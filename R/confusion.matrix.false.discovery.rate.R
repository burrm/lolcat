#' @rdname confusion.matrix.false.discovery.rate.simple
confusion.matrix.false.discovery.rate <- function(
  confusion.matrix
) {
  confusion.matrix.false.discovery.rate.simple(
    false.positive = confusion.matrix.false.positive(confusion.matrix)
    ,true.positive = confusion.matrix.true.positive(confusion.matrix)
  ) 
}