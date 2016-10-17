cor.point.biserial <- function(discrete_var, 
                               continuous_var,
                               alternative = c("two.sided","less","greater"),
                               conf.level = .95,
                               ordered = FALSE,
                               method = c("pearson.exact"
                                          #,"t.test" #TODO
                                          )
                               ) {
  if (!is.factor(discrete_var)) {
    discrete_var <- factor(discrete_var)
  }
  
  data <- data.frame(disc = discrete_var, cont = continuous_var)
  
  summary.out<- summary.impl(
    cont ~ disc
    ,data = data
    ,stat.n = T
    ,stat.mean =  T
    ,stat.sd = 1
    )
  
  if (!ordered) {
    summary.out <- summary.out[order(summary.out$mean),]
  }
  
  mean.g1 <- summary.out$mean[2]
  mean.g2 <- summary.out$mean[1]
  sd.g1 <- summary.out$sd[2]
  sd.g2 <- summary.out$sd[1]
  sd.all <- sqrt(var(data$cont))  
  n1 <- summary.out$n[2]
  n2 <- summary.out$n[1]
  n  <- n1 + n2
  
  p1 <- n1 / n
  p2 <- n2 / n

  r_pbi = ((mean.g1 - mean.g2) / sd.all) * sqrt(p1 * p2) * sqrt(n / (n-1))
  
  #Tate 1954/1955...
  se.est <- sqrt(  ((r_pbi^2 + 2*p1*(1-p1)*(2-3*r_pbi^2))/(4*n*p1*(1-p1))) * (1-r_pbi^2)^2  )
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ciupper <- r_pbi + cv*se.est
  cilower <- r_pbi - cv*se.est
  
  
    
  r_test <- cor.pearson.r.onesample.simple(sample.r = r_pbi, 
                                           sample.size = n, 
                                           null.hypothesis.rho = 0, 
                                           alternative = alternative, 
                                           conf.level = conf.level) 

    retval<-list(data.name   = "continuous variable and dichotomized variable",
               statistic   = r_test$statistic, 
               estimate    = c(r_pbi = r_pbi
                               ,r_test$estimate[2]
                               ,mean.g1 = mean.g1 
                               ,sd.g1 = sd.g1 
                               ,sample.size.g1 = n1
                               ,mean.g2 = mean.g2 
                               ,sd.g2 = sd.g2 
                               ,sample.size.g2 = n2
                               ,sd.all = sd.all
                               ,se.est.r_pbi = se.est
                               ),
               parameter   = 0,
               p.value     = r_test$p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Point Biserial Correlation Coefficient",
               conf.int    = c(cilower,ciupper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "point biserial correlation"
  names(retval$parameter) <- "null hypothesis point biserial correlation"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}