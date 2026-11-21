#' @rdname confusion.matrix.positive.predictive.value.simple
confusion.matrix.positive.predictive.value <- function(
  confusion.matrix
) {
confusion.matrix.positive.predictive.value.simple(
  true.positive = confusion.matrix.true.positive(confusion.matrix)
  ,false.positive = confusion.matrix.false.positive(confusion.matrix) 
) 
}

#' @rdname confusion.matrix.positive.predictive.value.simple
confusion.matrix.precision <- confusion.matrix.positive.predictive.value