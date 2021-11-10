#' Power - Single Sample Poisson Test  
#' 
#' Power calculation utilizes the Poisson distribution ("exact"" sample size/power).
#'
#' @param sample.size Scalar - sample size 
#' @param lambda.null.hypothesis Scalar - null hypothesis lambda parameter
#' @param lambda.alternative.hypothesis Scalar - alternative hypothesis lambda parameter
#' @param alpha Scalar - Type I error rate
#' @param alternative Scalar (character) - alternative hypothesis 
#' @param details Logical - Return calculation details (default) or return only power (details = FALSE)
#'
#' @return A data frame with details about the calculation or a single value with power (details = F). 

power.count.poisson.onesample.exact <- function(
  sample.size
  ,lambda.null.hypothesis
  ,lambda.alternative.hypothesis
  ,alpha = .05
  ,alternative = c("two.sided","less","greater")
  ,details = T
) {
  validate.htest.alternative(alternative = alternative)

  beta <- NA
  warning("Method Stub: Not implemented Yet...")

  if (alternative[1] == "two.sided") {
    #TODO
  } else if (alternative[1] == "greater") {
    if (lambda.alternative.hypothesis <= lambda.null.hypothesis) {
      #NA
    } else {
      #TODO
    }
  } else if (alternative[1] == "less") {
    if (lambda.alternative.hypothesis >= lambda.null.hypothesis) {
      #NA
    } else {
      #TODO
    }
  }
  
  pow <- 1-beta
  
  if (details) {
    as.data.frame(list(test="poisson"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = sample.size
                       ,actual = sample.size
                       ,lambda.null = lambda.null.hypothesis
                       ,lambda.alternative = lambda.alternative.hypothesis
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = pow
    ))
    
  }
  else {
    pow
  }
  
  
  
}

#power.mean.z.onesample(sample.size = 4, effect.size = .5, variance =  1)
