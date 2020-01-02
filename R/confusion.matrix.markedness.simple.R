confusion.matrix.markedness.simple <- function(
  #PPV inputs
  true.positive = 0
  ,false.positive = 1
  
  #NPV inputs
  ,true.negative = 0
  ,false.negative = 1
  
  ,positive.predictive.value = confusion.matrix.positive.predictive.value.simple(
    true.positive =  true.positive
    ,false.positive = false.positive
    )
  ,negative.predictive.value = confusion.matrix.negative.predictive.value.simple(
    true.negative = true.negative
    ,false.negative = false.negative
  )
) {
  positive.predictive.value + negative.predictive.value - 1
}