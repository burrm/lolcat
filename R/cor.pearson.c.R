cor.pearson.c <- function(x) {
  
  chi.sq.result <- chisq.test(x, correct = F)
  chi <- rmnames(chi.sq.result$statistic)
  
  c <- sqrt(chi/(chi+sum(x)))
  
  q<-min(nrow(x),ncol(x))
  
  retval<-list(data.name   = "data",
               statistic   = c(C = c), 
               estimate    = c( chi.square = rmnames(chi.sq.result$statistic)
                                ,chi.square.df = (nrow(x)-1)*(ncol(x)-1)
                                ,chi.square.p = rmnames(chi.sq.result$p.value)
                                ,max.possible.C = sqrt(q-1)/q
               ),
               parameter   = 0 ,
               p.value     = chi.sq.result$p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Pearson C and Pearson Chi-Squared"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "C"
  names(retval$parameter) <- "null hypothesis C"
  
  class(retval)<-"htest"
  retval
}