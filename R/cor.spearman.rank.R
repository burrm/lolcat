cor.spearman.rank <- function (x1
                               ,x2
                               ,conf.level = .95
                               ,alternative = c("two-sided","less","greater")) {
  
  r_sp <- cor(x1,x2,method="spearman")
  se.est <- 1/sqrt(length(x1)-1)
  
  z = r_sp/se.est
  
  cv  <- qnorm(conf.level+(1-conf.level)/2)
  
  upperci <- r_sp + cv*se.est
  lowerci <- r_sp - cv*se.est
  
  p.value <- if (alternative[1] == "two-sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  
  retval<-list(data.name   = "data",
               statistic   = c(r_sp = r_sp), 
               estimate    = c(se.est = se.est
               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Spearman Rank Correlation Coefficient",
               conf.int    = c(lowerci,upperci)
  )
  
  names(retval$null.value) <- "rho_sp"
  names(retval$parameter) <- "null hypothesis rho_sp"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
  
}