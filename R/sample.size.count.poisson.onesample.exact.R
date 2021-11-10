#' Sample Size - Single Sample Poisson Test  
#' 
#' Sample size calculation utilizes the square root transformation (approximate sample size/power).
#'
#' @param lambda.null.hypothesis Scalar - null hypothesis lambda parameter
#' @param lambda.alternative.hypothesis Scalar - alternative hypothesis lambda parameter
#' @param alpha Scalar - Type I error rate
#' @param beta Scalar - Type II error rate
#' @param alternative Scalar (character) - alternative hypothesis 
#' @param details Logical - Return calculation details (default) or return only sample size (details = FALSE)
#' @param power.from.actual Logical - If true, return 1-beta, if false, calculate power using calculated sample size.
#'
#' @return A data frame with details about the calculation or a single value with sample size (details = F). 

sample.size.count.poisson.onesample.exact <- function(
   lambda.null.hypothesis
  ,lambda.alternative.hypothesis
  ,alpha = .05
  ,beta  = .1
  ,alternative = c("two.sided","less","greater")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
) {
  validate.htest.alternative(alternative = alternative)
  
  n         <- NA

  warning("Method Stub: Not implemented Yet...")
  #TODO: Calculate sample size

  n.rounded <- ceiling(n)
  
  if (power.from.actual) {
    
    power <- 1-beta
    
  } else {
      
    power <- power.count.poisson.onesample.exact(
      sample.size                    = n.rounded 
      ,lambda.null.hypothesis        = lambda.null.hypothesis
      ,lambda.alternative.hypothesis = lambda.alternative.hypothesis
      ,alpha                         = alpha
      ,alternative                   = alternative
      ,details                       = FALSE
    )
    
    beta <- 1-power
    
  }
  
  if (details) {
  as.data.frame(list(test="poisson"
                     ,type = "one.sample"
                     ,alternative = alternative[1]
                     ,sample.size = n.rounded
                     ,actual = n
                     ,lambda.null = lambda.null.hypothesis
                     ,lambda.alternative = lambda.alternative.hypothesis
                     ,alpha = alpha
                     ,conf.level = 1-alpha
                     ,beta = beta
                     ,power = power
                     ))
  
  }
  else {
    n.rounded
  }
}


