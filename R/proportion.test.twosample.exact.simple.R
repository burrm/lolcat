#' Two Sample Proportion Test (Exact) 
#' 
#' Calculates a two-sample proportion test to determine if a samples  
#' come from different populations. Fisher Exact test is used for test computation.
#' 
#'
#' @param sample.proportion.g1 Scalar/numeric - Group 1 - sample proportion between 0 and 1
#' @param sample.size.g1 Scalar/numeric - Group 1 - sample size.
#' @param np.g1 Scalar/numeric - sample size multipled by sample proportion for group 1. Required if sample.proportion not specified.
#' @param sample.proportion.g2 Scalar/numeric - Group 2 - sample proportion between 0 and 1
#' @param sample.size.g2 Scalar/numeric - Group 2 - sample size.
#' @param np.g2 Scalar/numeric - sample size multipled by sample proportion for group 2. Required if sample.proportion not specified.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return Hypothesis test result showing results of test. 
proportion.test.twosample.exact.simple <- function(
  sample.proportion.g1 = NA
  ,sample.size.g1
  ,np.g1 = sample.proportion.g1 * sample.size.g1
  ,sample.proportion.g2 = NA
  ,sample.size.g2
  ,np.g2 = sample.proportion.g2 * sample.size.g2
  ,alternative = c("two.sided", "less", "greater")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  np.g1.rounded <- round(np.g1, 0)

  if (np.g1.rounded != np.g1) {
    warning("np.g1 rounded")
    np.g1 <- np.g1.rounded
  }
  
  if (is.na(sample.proportion.g1)) {
    sample.proportion.g1 <- np.g1 / sample.size.g1
  }


  np.g2.rounded <- round(np.g2, 0)

  if (np.g2.rounded != np.g2) {
    warning("np.g2 rounded")
    np.g2 <- np.g2.rounded
  }
  
  if (is.na(sample.proportion.g2)) {
    sample.proportion.g2 <- np.g2 / sample.size.g2
  }


  mat <- matrix(c(np.g1,sample.size.g1 - np.g1
                  ,np.g2,sample.size.g2 - np.g2), ncol = 2)
  
  fisher.out <- fisher.test(mat,alternative = alternative[1])
  
  g1.test <- proportion.test.onesample.exact.simple(np = np.g1
                                                    ,sample.size = sample.size.g1
                                                    ,null.hypothesis.proportion = .5
                                                    ,alternative = alternative[1]
                                                    ,conf.level = conf.level)
  

  g2.test <- proportion.test.onesample.exact.simple(np = np.g2
                                                    ,sample.size = sample.size.g2
                                                    ,null.hypothesis.proportion = .5
                                                    ,alternative = alternative[1]
                                                    ,conf.level = conf.level)
    
  
  
  retval<-list(data.name   = "sample proportions and sample sizes",
               statistic   = c(odds.ratio = rmnames(fisher.out$statistic)), 
               estimate    = c(sample.prop.g1 = sample.proportion.g1 
                               ,sample.size.g1 = sample.size.g1
                               ,n1.times.p1 = np.g1
                               ,n1.times.q1 = sample.size.g1 - np.g1
                               ,p.g1.lowerci = g1.test$conf.int[1]
                               ,p.g1.upperci = g1.test$conf.int[2]
                               
                               ,sample.prop.g2 = sample.proportion.g2 
                               ,sample.size.g2 = sample.size.g2
                               ,n2.times.p2 = np.g2
                               ,n2.times.q2 = sample.size.g2 - np.g2
                               ,p.g2.lowerci = g2.test$conf.int[1]
                               ,p.g2.upperci = g2.test$conf.int[2]
               ),
               parameter   = 1,
               p.value     = fisher.out$p.value,
               null.value  = 1,
               alternative = alternative[1],
               method      = "Two-Sample Proportion Test - Fisher Exact Test"
               ,conf.int    = c(fisher.out$conf.int[1],fisher.out$conf.int[2])
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$null.value) <- "odds ratio"
  names(retval$parameter) <- "null hypothesis odds ratio"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}