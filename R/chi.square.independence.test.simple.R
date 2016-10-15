chi.square.independence.test.simple <- function(
  observed.frequencies  #Matrix or data frame
  ,expected.frequencies = chi.square.2d.expected.frequencies(observed.frequencies) #Matrix or data frame
  ,alternative = c("greater", "less", "two.sided")
  ,conf.level = .95
) {
  chi.square <- sum(((observed.frequencies-expected.frequencies)^2)/expected.frequencies)
  df <- (nrow(observed.frequencies) -1) * (ncol(observed.frequencies) -1)
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  
  
  retval<-list(data.name   = "observed and expected frequencies",
               statistic   = chi.square, 
               estimate    = c(df = df
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Chi-Square Independence/Homogeneity Test",
               conf.int    = c(NA,NA)
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "difference"
  names(retval$parameter) <- "null hypothesis differences"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}

chi.square.homogeneity.test.simple <- chi.square.independence.test.simple
chi.square.contingency.table.test.simple <- chi.square.independence.test.simple