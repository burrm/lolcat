# Shapiro-Wilk Test for Exponentiality
# Implementer: Mike Burr
# Uses spline interpolation to to provide estimate of p value between points in the table.


shapiro.wilk.exponentiality.test.simple <- function(W, sample.size, alternative = c("two.sided", "less")) {
  validate.htest.alternative(alternative = alternative)
  
  p.value <- .shapiro.wilk.exponentiality.fn[[sample.size]](W)
  gt.50.pct <- W > .shapiro.wilk.exponentiality.table[(sample.size-2), 7]
  
  p.value <- ifelse(p.value < 0, 0, p.value)
  p.value <- ifelse(p.value > 1, 1, p.value)
  
  if (gt.50.pct) {
    p.value <- 1-p.value
  }
  
  if (alternative[1] == "two.sided") {
    #See pg 359 for method illustration in Shapiro/Wilk 1972.
    p.value <- 2* p.value
  } else { 
    
  }
  
  p.value <- ifelse(p.value < 0, 0, p.value)
  p.value <- ifelse(p.value > 1, 1, p.value)
  
  retval<-list(data.name   = "input data",
               statistic   = c(W = W), 
               estimate    = c(W = W, sample.size = sample.size),
               parameter   = 1 ,
               p.value     = p.value,
               null.value  = 1,
               alternative = alternative[1],
               method      = "Shapiro-Wilk Test for Exponentiality"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "W statistic"
  names(retval$parameter) <- "null hypothesis W statistic"
  
  class(retval)<-"htest"
  retval
}

