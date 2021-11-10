#' Power - Single Sample Poisson Test  
#' 
#' Power calculation utilizes the square root transformation (approximate sample size/power).
#'
#' @param sample.size Scalar - sample size 
#' @param lambda.null.hypothesis Scalar - null hypothesis lambda parameter
#' @param lambda.alternative.hypothesis Scalar - alternative hypothesis lambda parameter
#' @param alpha Scalar - Type I error rate
#' @param alternative Scalar (character) - alternative hypothesis 
#' @param details Logical - Return calculation details (default) or return only power (details = FALSE)
#'
#' @return A data frame with details about the calculation or a single value with power (details = F). 

power.count.poisson.onesample.approximate <- function(
  sample.size
  ,lambda.null.hypothesis
  ,lambda.alternative.hypothesis
  ,alpha = .05
  ,alternative = c("two.sided","less","greater")
  ,details = T
) {
  validate.htest.alternative(alternative = alternative)
  
  #z_alpha  
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = F)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided",alpha/2,alpha), lower.tail = T)   
  
  z.beta <- 2*sqrt(sample.size)*(sqrt(lambda.alternative.hypothesis)-sqrt(lambda.null.hypothesis))

  #print(paste0("z.upper + z.beta = ",z.upper + z.beta))
  #print(paste0("z.upper - z.beta = ",z.upper - z.beta))
  #print(paste0("z.lower + z.beta = ",z.lower + z.beta))
  #print(paste0("z.lower - z.beta = ",z.lower - z.beta))


  beta <- NA
  
  if (lambda.alternative.hypothesis < lambda.null.hypothesis) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.lower - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = F)
      
    } else if (alternative[1] == "greater") {

      beta <- pnorm(z.lower - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = T)
      
            
    } else {
      
      beta <- pnorm(z.lower - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = F)
      
      
    }  
  } else if (lambda.alternative.hypothesis >= lambda.null.hypothesis) {
    if (alternative[1] == "two.sided") {
      
      beta <- pnorm(z.upper - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = T)    
      
    } else if (alternative[1] == "greater") {
      
      beta <- pnorm(z.upper - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = T)
      
    } else {
      
      beta <- pnorm(z.upper - z.beta
                    #,mean=0
                    #,sd=1
                    ,lower.tail = F)
      
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
