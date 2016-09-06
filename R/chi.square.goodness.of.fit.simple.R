chi.square.goodness.of.fit.simple <- function(
  observed.frequencies
  ,expected.frequencies
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  chi.square <- sum(((observed.frequencies-expected.frequencies)^2)/expected.frequencies)
  df <- length(observed.frequencies)-1
  
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
               method      = "Chi-Square Goodness-of-Fit Test",
               conf.int    = c(NA,NA)
  )
  
  names(retval$statistic) <- "chi-square statistic"
  names(retval$null.value) <- "difference"
  names(retval$parameter) <- "null hypothesis differences"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}