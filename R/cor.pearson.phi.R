cor.pearson.phi <- function(x) {
  
  chi.sq.result <- chisq.test(x, correct = F)
  chi <- rmnames(chi.sq.result$statistic)
  
  phi <- sqrt(chi/sum(x))
  
  q <- min(nrow(x), ncol(x))
  
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
               estimate    = c( phi.squared = phi^2
                                #,max.possible.phi.squared = q-1
                                ,chi.square = rmnames(chi.sq.result$statistic)
                                ,chi.square.df = (nrow(x)-1)*(ncol(x)-1)
                                ,chi.square.p = rmnames(chi.sq.result$p.value)
                                
               ),
               parameter   = 0 ,
               p.value     = p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = ifelse(fisher.exact,
                                     "Pearson Phi and Fisher Exact Test"
                                    ,"Pearson Phi and Pearson Chi-Squared")#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "phi"
  names(retval$parameter) <- "null hypothesis phi"
  
  class(retval)<-"htest"
  retval
}