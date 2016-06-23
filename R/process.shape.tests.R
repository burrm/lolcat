process.shape.tests <- function(x
                                
                                ,all.tests = F
                                ,all.normality.tests = F
                                
                                #Tests to return
                                ,stat.ad.test   = F              || all.tests || all.normality.tests
                                ,stat.sw.test   = F              || all.tests || all.normality.tests
                                ,stat.lin.mudholkar.test = F     || all.tests || all.normality.tests
                                ,stat.jarque.bera.test = F       || all.tests || all.normality.tests
                                ,stat.dagostino.omni.test = F    || all.tests || all.normality.tests
                                ,stat.skew.test = T              || all.tests || all.normality.tests
                                ,stat.kurt.test = T              || all.tests || all.normality.tests
                                ,stat.pois.dist.test = F         || all.tests 
                                ,stat.sw.exp.test = F            || all.tests 

                                ) {

  orig.warnings <- unlist(options("warn"))
  options(warn = -1)
  
  
  ret <- list()
    
  if (stat.ad.test) {
    t1 <- anderson.darling.test(x)
    ret$adtest.AA <- rmnames(t1$statistic)
    ret$adtest.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.sw.test) {
    t1 <- shapiro.test(x)
    ret$swtest.W <- rmnames(t1$statistic)
    ret$swtest.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.dagostino.omni.test) {
    t1 <- dagostino.normality.omnibus.test(x)
    ret$dago.omni.chi.sq <- rmnames(t1$statistic)
    ret$dago.omni.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.skew.test) {
    t1 <- skewness.test(x)
    ret$g3.skewness <- rmnames(t1$statistic)
    ret$g3test.z <- rmnames(t1$estimate[2])
    ret$g3test.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.kurt.test) {
    t1 <- kurtosis.test(x)
    ret$g4.kurtosis <- rmnames(t1$statistic)
    ret$g4test.z <- rmnames(t1$estimate[2])
    ret$g4test.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.pois.dist.test) {
    t1 <- poisson.dist.test(x)
    ret$pois.test.chi.square <- rmnames(t1$statistic)
    ret$pois.test.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.sw.exp.test) {
    t1 <- shapiro.wilk.exponentiality.test(x)
    ret$sw.exp.test.W <- rmnames(t1$statistic) 
    ret$sw.exp.test.p <- t1$p.value
    rm(t1)
  }
  
  if (stat.lin.mudholkar.test) {
    t1 <- lin.mudholkar.normality.test(x)
    ret$lin.mudholkar.test.r <- rmnames(t1$estimate[2]) 
    ret$lin.mudholkar.test.z <- rmnames(t1$statistic) 
    ret$lin.mudholkar.test.p <- t1$p.value
    rm(t1)
  }

  if (stat.jarque.bera.test) {
    t1 <- jarque.bera.normality.test(x)
    ret$jarque.bera.test.chi.square <- rmnames(t1$statistic) 
    ret$jarque.bera.test.p          <- t1$p.value
    rm(t1)
  }
  
  
  
  options(warn = orig.warnings)
  
  ret
}