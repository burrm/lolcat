# Shapiro-Wilk Test for Exponentiality
# Implementer: Mike Burr
# Uses spline interpolation to to provide estimate of p value between points in the table.


# Source:
# An Analysis of Variance Test for the Exponential Distribution (Complete Samples) 
# Author(s): S. S. Shapiro and M. B. Wilk 
# Source: Technometrics, Vol. 14, No. 2 (May, 1972), pp. 355-370

shapiro.wilk.exponentiality.test <- function(x, alternative = c("two.sided", "less") ) {
  validate.htest.alternative(alternative = alternative)
  x <- na.omit(x)
  sample.size <- max(length(x),1)
  
  x.bar <- mean(x)
  x.min <- min(x)
  sse <- sum((x-x.bar)^2)
  
  
  if (sample.size > 100) {
    sample.size <- 100
    warn("shapiro.wilk.exponentiality.test should be used for sample sizes 3 to 100")
  }
  
  W <- sample.size*(x.bar-x.min)^2/((sample.size-1)*sse)
  
  shapiro.wilk.exponentiality.test.simple(W = W, sample.size = sample.size, alternative = alternative)
}

#shapiro.wilk.exponentiality.test(c(6,1,-4,8,-2,5,0))
#shapiro.wilk.exponentiality.test(c(1,100,2))

#Example 1 - less than 5%
#shapiro.wilk.exponentiality.test.simple(.0259, 20)

#Example 2 - pretty close, compare 32% with "about 31%"
#shapiro.wilk.exponentiality.test.simple(.127, 14)

