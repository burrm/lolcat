#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.true.positive.rate <- function(
  confusion.matrix
) {
confusion.matrix.true.positive.rate.simple(
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.negative = confusion.matrix.false.negative(confusion.matrix)
) 
}

#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.sensitivity <- confusion.matrix.true.positive.rate
#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.recall      <- confusion.matrix.true.positive.rate
#' @rdname confusion.matrix.true.positive.rate.simple
confusion.matrix.hit.rate    <- confusion.matrix.true.positive.rate
