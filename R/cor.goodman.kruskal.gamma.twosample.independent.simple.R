cor.goodman.kruskal.gamma.twosample.independent.simple <- function(
  gamma.g1
  ,se.est.gamma.g1
  ,gamma.g2
  ,se.est.gamma.g2
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  z <- (gamma.g1 - gamma.g2)/sqrt(se.est.gamma.g1 + se.est.gamma.g2)
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  retval<-list(data.name   = "independent sample G values",
               statistic   = z, 
               estimate    = c(G.g1 = gamma.g1 
                               ,se.est.g1 = se.est.gamma.g1
                               ,G.g2 = gamma.g2 
                               ,se.est.g2 = se.est.gamma.g2
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Two-Sample Independent Goodman and Kruskal's Gamma (G)",
               conf.int    = c(NA, NA)
  )
  
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "difference"
  names(retval$parameter) <- "null hypothesis difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}