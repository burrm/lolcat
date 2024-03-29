#' @rdname proportion.test.onesample.approximate.simple
proportion.test.onesample.approximate <- function(
  x 
  ,success.value = 1 #Can be anything that compares with x with ==
  ,null.hypothesis.proportion = .5
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
  ,continuity.correction = T
) {
  validate.htest.alternative(alternative = alternative)
  x <- na.omit(x)
  count.success <- length(which(x == success.value))
  sample.size <- length(x)
  
  proportion.test.onesample.approximate.simple(
    sample.proportion = count.success / sample.size
    ,sample.size = sample.size
    ,null.hypothesis.proportion = null.hypothesis.proportion
    ,alternative = alternative
    ,conf.level = conf.level
  )
}