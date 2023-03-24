#' Power - Single Sample Poisson Test  
#' 
#' Power calculation utilizes the Poisson distribution ("exact" sample size/power).
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

  pow <- NA

  if (alternative[1] == "two.sided") {
    if (lambda.alternative.hypothesis == lambda.null.hypothesis) {
      #NA
    } else {
      a.sample.size = sample.size
      a.lambda.null.hypothesis = lambda.null.hypothesis
      a.lambda.alternative.hypothesis = lambda.alternative.hypothesis
      a.alpha = alpha
      a.alternative = if(lambda.alternative.hypothesis < lambda.null.hypothesis) {
          "less"
        } else {
          "greater"
        }

      pow <- power.count.poisson.onesample.exact(
        sample.size = a.sample.size
        ,lambda.null.hypothesis = a.lambda.null.hypothesis
        ,lambda.alternative.hypothesis = a.lambda.alternative.hypothesis
        ,alpha = a.alpha/2
        ,alternative = a.alternative
        ,details = T
      )
      
      alpha <- 2*rmnames(pow$alpha[1])
      pow   <- rmnames(pow$power[1])
    }
  } else if (alternative[1] == "greater") {
    if (lambda.alternative.hypothesis <= lambda.null.hypothesis) {
      #NA
    } else {
      table.lambda.alternative.hypothesis <- table.dist.poisson(
        lambda = sample.size*lambda.alternative.hypothesis
      )
      table.lambda.null.hypothesis        <- table.dist.poisson(
        lambda = sample.size*lambda.null.hypothesis,
        include.x = nrow(table.lambda.alternative.hypothesis)
      )

      idx   <- which(table.lambda.null.hypothesis$eq.and.above <= alpha)
      pow   <- table.lambda.alternative.hypothesis$eq.and.above[min(idx)+1]
      alpha <- table.lambda.null.hypothesis$eq.and.above[min(idx)]
    }
  } else if (alternative[1] == "less") {
    if (lambda.alternative.hypothesis >= lambda.null.hypothesis) {
      #NA
    } else {
      table.lambda.null.hypothesis        <- table.dist.poisson(
        lambda = sample.size*lambda.null.hypothesis
      )
      table.lambda.alternative.hypothesis <- table.dist.poisson(
        lambda = sample.size*lambda.alternative.hypothesis,
        include.x = nrow(table.lambda.null.hypothesis)
      )

      idx   <- which(table.lambda.null.hypothesis$eq.and.below <= alpha)
      alpha <- table.lambda.null.hypothesis$eq.and.below[length(idx)]
      pow   <- table.lambda.alternative.hypothesis$eq.and.below[length(idx)-1]
    }
  }
  
  beta <- 1-pow
  
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