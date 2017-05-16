cor.tschuprow.tc <- function(x) {
  
  chi.sq.result <- chisq.test(x, correct = F)
  chi <- rmnames(chi.sq.result$statistic)
  
  p <- nrow(x) - 1
  q <- ncol(x) - 1

  t.c <- sqrt(chi / (sum(x) * sqrt(p*q)))

  retval<-list(data.name   = "data",
               statistic   = c(t.c = t.c), 
               estimate    = c( max.possible.t.c = min(p,q)/max(p,q)
                                #,max.possible.phi.squared = q-1
                                ,chi.square = rmnames(chi.sq.result$statistic)
                                ,chi.square.df = (nrow(x)-1)*(ncol(x)-1)
                                ,chi.square.p = rmnames(chi.sq.result$p.value)
                                
               ),
               parameter   = 0 ,
               p.value     = chi.sq.result$p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Pearson Phi and Pearson Chi-Squared"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "phi"
  names(retval$parameter) <- "null hypothesis phi"
  
  class(retval)<-"htest"
  retval
}

 
cor.chuprov.tc <- cor.tschuprow.tc