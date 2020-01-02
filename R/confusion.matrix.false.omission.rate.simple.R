#false omission rate
confusion.matrix.false.omission.rate.simple <- function(
  false.negative = 0
  ,true.negative = 1 
) {
  false.negative / (false.negative + true.negative)
}
