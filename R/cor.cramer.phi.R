cor.cramer.phi <- function(x) {
  
  chi.sq.result <- chisq.test(x, correct = F)
  chi <- rmnames(chi.sq.result$statistic)
  
  phi <- sqrt(chi/(sum(x)*min(nrow(x)-1,ncol(x)-1)))

  p.value <- chi.sq.result$p.value
  
  fisher.exact <- F
  if (nrow(x) == 2 && ncol(x) == 2) {
    #Evaluate via Fisher Exact Test
    fisher.exact <- T
    
    x <- xt.sums(x)
    
    p.value <- rmnames(
      proportion.test.twosample.exact.simple(
        sample.proportion.g1 = x[1,1]/x[1,3]
        ,sample.size.g1 = x[1,3]
        ,sample.proportion.g2 = x[2,1]/x[2,3]
        ,sample.size.g2 = x[2,3]
      )$p.value
    )
  }
  
    
  retval<-list(data.name   = "data",
               statistic   = c(phi = phi), 
               estimate    = c( chi.square = rmnames(chi.sq.result$statistic)
                                ,chi.square.p = rmnames(chi.sq.result$p.value)
                               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = ifelse(fisher.exact 
                                    ,"Cramer's Phi and Fisher Exact Test"
                                    ,"Cramer's Phi and Pearson Chi-Squared"
                                    )#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "phi"
  names(retval$parameter) <- "null hypothesis phi"

  class(retval)<-"htest"
  retval
}