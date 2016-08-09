skewness.test <-
function(x
         ,conf.level = .95
         ,alternative = c("two.sided","less","greater")
         )
{
  x <- na.omit(x)
  
  fisher.g1   <- skewness(x)
  sample.size <- length(x)
  
  skewness.test.simple(skewness =  fisher.g1,
                       sample.size = sample.size,
                       conf.level = conf.level,
                       alternative = alternative)
  
}
