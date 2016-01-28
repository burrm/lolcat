cor.cramer.phi <- function(x) {
  
  chi.sq.result <- chisq.test(x, correct = F)
  chi <- rmnames(chi.sq.result$statistic)
  
  phi <- sqrt(chi/(sum(x)*min(nrow(x)-1,ncol(x)-1)))
  
  retval<-list(data.name   = "data",
               statistic   = c(phi = phi), 
               estimate    = c( chi.square = rmnames(chi.sq.result$statistic)
                                ,chi.square.p = rmnames(chi.sq.result$p.value)
                               ),
               parameter   = 0 ,
               p.value     = chi.sq.result$p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Cramer's Phi and Pearson Chi-Squared"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "phi"
  names(retval$parameter) <- "null hypothesis phi"

  class(retval)<-"htest"
  retval
}