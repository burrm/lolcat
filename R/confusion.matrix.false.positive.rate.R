#' @rdname confusion.matrix.false.positive.rate.simple
confusion.matrix.false.positive.rate <- function(
  confusion.matrix
) {
  confusion.matrix.false.positive.rate.simple(
  false.positive = confusion.matrix.false.positive(confusion.matrix)
  ,true.negative = confusion.matrix.true.negative(confusion.matrix)
  ) 
}

#' @rdname confusion.matrix.false.positive.rate.simple
confusion.matrix.fall.out <- confusion.matrix.false.positive.rate