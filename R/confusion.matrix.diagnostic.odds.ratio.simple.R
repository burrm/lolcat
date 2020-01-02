confusion.matrix.diagnostic.odds.ratio.simple <- function(
  #TPR inputs
  true.positive = 0
  ,false.negative = 1 
  ,count.positive = true.positive + false.negative
  
  #FPR inputs
  ,false.positive = 0
  ,true.negative = 1 
  ,count.negative = false.positive + true.negative  
  
  #FNR inputs - already via TPR above
  
  #TNR inputs already via FPR above

  #Only true required parameters:
  ,true.positive.rate = confusion.matrix.true.positive.rate.simple(
    true.positive = true.positive
    ,count.positive = count.positive
  )
  ,false.positive.rate = confusion.matrix.false.positive.rate.simple(
    false.positive = false.positive
    ,count.negative = count.negative
  )  
  
  ,true.negative.rate =confusion.matrix.true.negative.rate.simple(
    true.negative = true.negative
    ,count.negative = count.negative
  )
  ,false.negative.rate = confusion.matrix.false.negative.rate.simple(
    false.negative = false.negative
    ,count.positive = count.positive
  )
  
  ,positive.likelihood.ratio = confusion.matrix.positive.likelihood.ratio.simple(
    true.positive.rate = true.positive.rate
    ,false.positive.rate = false.positive.rate
  )  
  
  ,negative.likelihood.ratio = confusion.matrix.negative.likelihood.ratio.simple(
    true.negative.rate = true.negative.rate
    ,false.negative.rate = false.negative.rate
  )
    
) {
  positive.likelihood.ratio / negative.likelihood.ratio
}