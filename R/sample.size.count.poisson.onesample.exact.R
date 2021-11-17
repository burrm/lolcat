#' Sample Size - Single Sample Poisson Test  
#' 
#' Sample size calculation utilizes the exact power calculation.
#'
#' @param lambda.null.hypothesis Scalar - null hypothesis lambda parameter
#' @param lambda.alternative.hypothesis Scalar - alternative hypothesis lambda parameter
#' @param alpha Scalar - Type I error rate
#' @param beta Scalar - Type II error rate
#' @param alternative Scalar (character) - alternative hypothesis 
#' @param n.initial Scalar - Integer with initial sample size guess.
#' @param max.iteration Scalar - Maximum iterations to perform before declaring failure.
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

  ,n.initial = sample.size.count.poisson.onesample.approximate(
      lambda.null.hypothesis = lambda.null.hypothesis,
      lambda.alternative.hypothesis = lambda.alternative.hypothesis,
      alpha = alpha,
      beta = beta,
      alternative = alternative,
      details = FALSE
    )
  ,max.iteration = 10000
) {
  validate.htest.alternative(alternative = alternative)
  
  n         <- n.initial

  if (alternative[1] == "greater" && lambda.alternative.hypothesis < lambda.null.hypothesis) {
    stop("bad alternative hypothesis specified for given null and alternative hypothesis lambdas, try \"less\"")
  }
  
  if (alternative[1] == "less" && lambda.alternative.hypothesis > lambda.null.hypothesis) {
    stop("bad alternative hypothesis specified for given null and alternative hypothesis lambdas, try \"greater\"")
  }

  if (!is.finite(n)) {
    n <- 1
  }

  
  
  iter <- 0
  this.beta <- power.count.poisson.onesample.exact(
      sample.size                    = n 
      ,lambda.null.hypothesis        = lambda.null.hypothesis
      ,lambda.alternative.hypothesis = lambda.alternative.hypothesis
      ,alpha                         = alpha
      ,alternative                   = alternative
      ,details                       = TRUE
  )$beta[1]

  if (this.beta > beta) {
    #increase sample size

    while (this.beta > beta & iter < max.iteration) {
      iter <- iter + 1
      n <- n + 1

      this.beta <- power.count.poisson.onesample.exact(
        sample.size                    = n 
        ,lambda.null.hypothesis        = lambda.null.hypothesis
        ,lambda.alternative.hypothesis = lambda.alternative.hypothesis
        ,alpha                         = alpha
        ,alternative                   = alternative
        ,details                       = TRUE
      )$beta[1]
    }
  } else if (this.beta < beta) {
    #try decreasing sample size
    while (n > 1 & this.beta < beta & iter < max.iteration) {
      iter <- iter + 1
      n <- n - 1

      this.beta <- power.count.poisson.onesample.exact(
        sample.size                    = n 
        ,lambda.null.hypothesis        = lambda.null.hypothesis
        ,lambda.alternative.hypothesis = lambda.alternative.hypothesis
        ,alpha                         = alpha
        ,alternative                   = alternative
        ,details                       = TRUE
      )$beta[1]
    }

    if (this.beta >= beta) {
      n <- n+1
    }
  }

  if (iter >= max.iteration-1) {
    warning("max.iteration reached without beta converging.")
  }

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


