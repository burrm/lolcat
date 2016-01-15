variance.test.twosample.independent <- function(
   g1
  ,g2
  ,alternative = c("two-sided","less","greater")
  ,conf.level = 0.95
  ,assume.normality = c("yes", "no")
) {
  if (assume.normality[1] == "yes") {
    #F Test
    n.g1 <- length(g1)
    n.g2 <- length(g2)
    
    sample.variance.g1<- var(g1)
    sample.variance.g2<- var(g2)
    
    variance.test.twosample.independent.simple(
                                               sample.variance.g1
                                               ,n.g1
                                               ,sample.variance.g2
                                               ,n.g2
                                               ,alternative
                                               ,conf.level
                                               )
  } else {
    #T Test on ADMn-1
    g1.ADMn1 <- na.omit(dispersion.ADMn1(g1))
    g2.ADMn1 <- na.omit(dispersion.ADMn1(g2))
    
    mean.g1<-mean(g1.ADMn1)
    mean.g2<-mean(g2.ADMn1)
    
    var.g1<-var(g1.ADMn1)
    var.g2<-var(g2.ADMn1)
    
    n.g1 <- length(g1.ADMn1)
    n.g2 <- length(g2.ADMn1)
    
    retval <- t.test.twosample.independent.simple(sample.mean.g1 = mean.g1
                                                  ,sample.variance.g1 = var.g1
                                                  ,n.g1 = n.g1
                                                  ,sample.mean.g2 = mean.g2
                                                  ,sample.variance.g2 = var.g2
                                                  ,n.g2 = n.g2
                                                  ,h0.difference = 0
                                                  ,alternative = alternative
                                                  ,assume.equal.variances = "yes"
                                                  ,conf.level = conf.level
                                                  ,var.test.details = F
                                                  )
    
    retval$data.name <- "ADMn-1 values for samples"
    retval$method    <- "Two-Sample t Test for Equality of ADMn-1"
    
    retval
  }
}