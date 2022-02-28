#' Biserial Correlation Coefficient 
#' 
#' Calculate the biserial correlation coefficient, a correlation between a discrete/nominal variable and a continuous measure.
#'
#' @param discrete_var A vector of values corresponding to the discrete/nominal variable
#' @param continuous_var A vector of values corresponding to the continuous variable
#' @param alternative Alternative hypothesis directionality
#' @param conf.level Confidence level for calculating confidence intervals
#'
#' @return Hypothesis test result showing results of test. 
cor.biserial <- function(discrete_var
                         ,continuous_var
                         ,alternative = c("two.sided","less","greater")
                         ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  if (!is.factor(discrete_var)) {
    discrete_var <- factor(discrete_var)
  }
  
  data <- data.frame(disc = discrete_var, cont = continuous_var)
  
  summary.out<- summary.impl(
    cont ~ disc
    ,data = data
    ,stat.n = T
    ,stat.mean =  T
  )

  #summary.out <- summary.out[order(summary.out$n, decreasing = T),]
  
  #print(summary.out)
  


  mean.g1 <- summary.out$mean[2]
  mean.g2 <- summary.out$mean[1]
  sd.all <- sqrt(var(data$cont))  
  n  <- sum(summary.out$n)

  p.g1 <- summary.out$n[2] / n
  p.g2 <- summary.out$n[1] / n

  h <- dnorm(qnorm(max(p.g1, p.g2)))
  
  r_b = ((mean.g1 - mean.g2) / sd.all) * (p.g1*p.g2/h) * sqrt(n/(n-1))
  
  z <- h * r_b / sqrt(p.g1*p.g2/n) 

  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "continuous variable and dichotomized variable",
               statistic   = c(z =z), 
               estimate    = c(r_b = r_b
                               ,h = h
                               ,sample.size = n
                               ,sd.all = sd.all
                               ,mean.g1 = mean.g1
                               ,prop.g1 = p.g1
                               ,mean.g2 = mean.g2
                               ,prop.g2 = p.g2
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Biserial Correlation Coefficient",
               conf.int    = c(NA,NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "biserial correlation"
  names(retval$parameter) <- "null hypothesis biserial correlation"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}