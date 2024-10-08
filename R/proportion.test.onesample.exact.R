#' @rdname proportion.test.onesample.exact.simple
proportion.test.onesample.exact <- function(
  x 
  ,success.value = 1 #Can be anything that compares with x with ==
  ,null.hypothesis.proportion = .5
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,round.np = T
) {
  validate.htest.alternative(alternative = alternative)
  x <- na.omit(x)
  count.success <- length(which(x == success.value))
  sample.size <- length(x)
  
  proportion.test.onesample.exact.simple(
    sample.proportion = count.success / sample.size
    ,sample.size = sample.size
    ,null.hypothesis.proportion = null.hypothesis.proportion
    ,alternative = alternative
    ,conf.level = conf.level
    ,round.np = round.np
  )
}