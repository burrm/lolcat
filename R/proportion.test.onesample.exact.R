proportion.test.onesample.exact <- function(
  x 
  ,success.value = 1 #Can be anything that compares with x with ==
  ,h0.proportion = .5
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
) {
  x <- na.omit(x)
  count.success <- length(which(x == success.value))
  sample.size <- length(x)
  
  proportion.test.onesample.exact.simple(
    sample.proportion = count.success / sample.size
    ,sample.size = sample.size
    ,h0.proportion = h0.proportion
    ,alternative = alternative
    ,conf.level = conf.level
  )
}