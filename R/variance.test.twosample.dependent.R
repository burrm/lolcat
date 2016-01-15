variance.test.twosample.dependent <- function(
   g1
  ,g2
  ,alternative = c("two-sided","less","greater")
  ,conf.level = 0.95
  ,h0.difference = 0
  ,assume.normality = c("yes", "no")
  ,non.norm.method = c("meandiff", "dbar")
) {
  
  if (assume.normality[1] == "yes") {
    #Matched Pairs t Test for Variances

    
    variance.test.twosample.dependent.simple(sample.variance.g1 = var(g1)
                                             ,sample.variance.g2 = var(g2)
                                             ,n = length(g1)
                                             ,rho.estimate = cor(g1,g2)
                                             ,h0.difference = h0.difference
                                             ,alternative = alternative
                                             ,conf.level = conf.level
                                             )
    
  } else {
    
    if (non.norm.method[1] == "meandiff") {
      #Matched Pairs t Test on ADMn-1
      g1.ADMn1 <- dispersion.ADMn1(g1)
      g2.ADMn1 <- dispersion.ADMn1(g2)
      
      data<-data.frame(g1 = g1.ADMn1, g2 = g2.ADMn1)
      data<-na.omit(data)
      
      rho.estimate<-cor(data$g1,data$g2)
      
      retval <- t.test.twosample.dependent.simple.meandiff(sample.mean.g1 = mean(data$g1)
                                                         ,sample.variance.g1 = var(data$g1)
                                                         ,sample.mean.g2 = mean(data$g2)
                                                         ,sample.variance.g2 = var(data$g2)
                                                         ,n=nrow(data)
                                                         ,rho.estimate = rho.estimate
                                                         ,h0.difference = h0.difference
                                                         ,alternative = alternative
                                                         ,conf.level = conf.level
                                                         ,assume.equal.variances = "yes"
                                                         ,var.test.details = F
                                                         )
      
      retval$method    <- "Two-Sample Dependent t Test for Equality of ADMn-1 (Mean-Difference Method)"
    } else if (non.norm.method[1] == "dbar") {
      g1.ADMn1 <- dispersion.ADMn1(g1)
      g2.ADMn1 <- dispersion.ADMn1(g2)
      
      diff.g12 <- na.omit(g1.ADMn1 - g2.ADMn1)
      
      
      retval <- t.test.twosample.dependent.simple.dbar(pair.differences.mean = mean(diff.g12)
                                                       ,pair.differences.variance = var(diff.g12)
                                                       ,n = length(diff.g12)
                                                       ,h0.difference = h0.difference
                                                       ,alternative = alternative
                                                       ,conf.level = conf.level
                                                       )
      
      retval$method    <- "Two-Sample Dependent t Test for Equality of ADMn-1 (D-Bar Method)"
      
    } else {
      stop("Unknown non.norm.method in variance.test.twosample.dependent")
    }

    retval$data.name <- "ADMn-1 values for samples"
    retval
  }
}