#' D'Agostino's Normality Test - Omnibus 
#' 
#' Calculates a statistical test to determine if normality assumption for the sample is rejected.
#' 
#' @param x Vector/numeric - sample data.
#' @param skewness Scalar/numeric - sample skewness.
#' @param kurtosis Scalar/numeric - sample kurtosis.
#' @param sample.size Scalar/numeric - sample size.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return Hypothesis test result showing results of test. 
dagostino.normality.omnibus.test.simple <- function(skewness
                                                    ,kurtosis
                                                    ,sample.size
                                                    ,input = c("fisher") #todo - pearson
                                                    ,conf.level = .95
                                                    ,alternative = #c("two.sided") #,"less",
                                                      "greater"#)
                                                    
)
{
  validate.htest.alternative(alternative = alternative)
  sk.test <- skewness.test.simple(skewness, sample.size, conf.level = conf.level)
  ku.test <- kurtosis.test.simple(kurtosis, sample.size, conf.level = conf.level)
  
  z_sk <- rmnames(sk.test$estimate[2])
  z_ku <- rmnames(ku.test$estimate[2])
  
  chi.sq <- z_sk^2 + z_ku^2
  if (any(!is.finite(c(z_sk, z_ku)))) {
    chi.sq <- NA
  }
  
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
  
  estimate <- c(  g3.z.square = rmnames(sk.test$estimate[2]^2)
                  ,g4.z.square = rmnames(ku.test$estimate[2]^2)
                  ,df          = rmnames(df)
                  ,sample.size = sample.size
                  ,g3.z        = rmnames(sk.test$estimate[2])
                  ,g3.p.value  = rmnames(sk.test$p.value)
                  ,g3.skewness = rmnames(sk.test$estimate[1])
                  ,g3.root.b1  = rmnames(sk.test$estimate[4])
                  ,g3.lowerci  = rmnames(sk.test$conf.int[1])
                  ,g3.upperci  = rmnames(sk.test$conf.int[2])
                  ,g4.z        = rmnames(ku.test$estimate[2])
                  ,g4.p.value  = rmnames(ku.test$p.value)
                  ,g4.kurtosis = rmnames(ku.test$estimate[1])
                  ,g4.b2       = rmnames(ku.test$estimate[4])
                  ,g4.lowerci  = rmnames(ku.test$conf.int[1])
                  ,g4.upperci  = rmnames(ku.test$conf.int[2])
                  
  )
  
  retval<-list(data.name   = "input data",
               statistic   = c(chi.square = chi.sq), 
               estimate    = estimate,
               parameter   = df,
               p.value     = p.value,
               null.value  = df,
               alternative = alternative[1],
               method      = "D'Agostino Omnibus Normality Test ",
               conf.int    = c(chilower,chiupper)
  )
  
  #names(retval$estimate) <- c("sample skewness","z statistic")
  
  names(retval$null.value) <- "chi.square"
  names(retval$parameter) <- "null hypothesis chi.square"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}