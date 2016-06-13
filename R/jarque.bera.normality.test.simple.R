jarque.bera.normality.test.simple <- function(skewness
                                              ,kurtosis
                                              ,sample.size
                                              ,input = c("fisher") #todo - pearson
                                              ,conf.level = .95
                                              ,alternative = #c("two.sided") #,"less",
                                              "greater"#)
                                                    
)
{
  sk.test <- skewness.test.simple(skewness, sample.size, conf.level = conf.level)
  ku.test <- kurtosis.test.simple(kurtosis, sample.size, conf.level = conf.level)
  
  chi.sq <- sample.size*((sk.test$estimate[4]^2)/6 + (ku.test$estimate[4] -3)^2/24)
  df <- 2
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.sq,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.sq,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.sq,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  chilower = NA #qchisq((1 - conf.level)/2, df)
  chiupper = NA #qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  
  estimate <- c(  #g3.z.square = rmnames(sk.test$estimate[2]^2)
                  #,g4.z.square = rmnames(ku.test$estimate[2]^2)
                  df          = rmnames(df)
                  ,sample.size = sample.size
                  #,g3.z        = rmnames(sk.test$estimate[2])
                  #,g3.p.value  = rmnames(sk.test$p.value)
                  ,g3.skewness = rmnames(sk.test$estimate[1])
                  ,g3.root.b1  = rmnames(sk.test$estimate[4])
                  #,g3.lowerci  = rmnames(sk.test$conf.int[1])
                  #,g3.upperci  = rmnames(sk.test$conf.int[2])
                  #,g4.z        = rmnames(ku.test$estimate[2])
                  #,g4.p.value  = rmnames(ku.test$p.value)
                  ,g4.kurtosis = rmnames(ku.test$estimate[1])
                  ,g4.b2       = rmnames(ku.test$estimate[4])
                  #,g4.lowerci  = rmnames(ku.test$conf.int[1])
                  #,g4.upperci  = rmnames(ku.test$conf.int[2])
                  
  )
  
  retval<-list(data.name   = "input data",
               statistic   = c(chi.square = chi.sq), 
               estimate    = estimate,
               parameter   = df,
               p.value     = p.value,
               null.value  = df,
               alternative = alternative[1],
               method      = "Jarque-Bera Normality Test ",
               conf.int    = c(chilower,chiupper)
  )
  
  #names(retval$estimate) <- c("sample skewness","z statistic")
  
  names(retval$null.value) <- "chi.square"
  names(retval$parameter) <- "null hypothesis chi.square"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}