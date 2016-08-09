t.test.twosample.independent <- function(g1, 
                                         g2, 
                                         ... #additional options for t.test.twosample.independent.simple
) {
  sample.mean.g1 <- mean(g1)
  sample.mean.g2 <- mean(g2)
  sample.variance.g1 <- var(g1)
  sample.variance.g2 <- var(g2)
  sample.size.g1<-length(na.omit(g1))
  sample.size.g2<-length(na.omit(g2))
  
  t.test.twosample.independent.simple(sample.mean.g1 = sample.mean.g1
                                      ,sample.variance.g1 = sample.variance.g1
                                      ,sample.size.g1 = sample.size.g1
                                      ,sample.mean.g2 = sample.mean.g2
                                      ,sample.variance.g2 = sample.variance.g2
                                      ,sample.size.g2 = sample.size.g2
                                      ,...
  )
  
  
}