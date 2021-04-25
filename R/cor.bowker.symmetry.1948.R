cor.bowker.symmetry.1948 <- function(
  observed.frequencies #matrix
  ,alternative = c("greater", "two.sided", "less") #Paper identifies it as a one-tail (greater) test
#  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  m <- nrow(observed.frequencies)
  df = (m*(m - 1))/2
  
  chi.square.statistic <- 0
  
  for (i in 1:m) {
    for (j in 1:m) {
      if (i > j) {
        n_ij <- observed.frequencies[i,j]
        n_ji <- observed.frequencies[j,i]
        
        add_to <- ( (n_ij - n_ji)^2 / (n_ij + n_ji) )
        
        if (is.finite(add_to)) {
           chi.square.statistic <- chi.square.statistic + add_to
        }
      }
    }
  }

  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square.statistic,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square.statistic,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square.statistic,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "observed frequencies",
               statistic   = chi.square.statistic, 
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Bowker's Test of Symmetry (1948)"
  #             conf.int    = c(df*v/chiupper,df*v/chilower)
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "off-diagonal differences"
  names(retval$parameter) <- "null hypothesis off-diagonal differences"
  #attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
    
}