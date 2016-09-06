cor.cohen.kappa.onesample <- function(
  observed.frequencies #matrix
  ,expected.frequencies = chi.square.2d.expected.frequencies(observed.frequencies)
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  diag.observed <- diag(observed.frequencies)
  sum.diag.observed <- sum(diag.observed)
  
  diag.expected <- diag(expected.frequencies)
  sum.diag.expected <- sum(diag.expected)
  
  n <- sum(observed.frequencies)
  
  kappa    <- (sum.diag.observed - sum.diag.expected)/(n - sum.diag.expected)
  se.kappa <- sqrt((sum.diag.observed*(n-sum.diag.observed)) / ( n*(n-sum.diag.expected)^2  ) )
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ci.add  <- cv*se.kappa
  
  ci.lower <- kappa-ci.add
  ci.upper <- kappa+ci.add
  
  se.kappa.dist <- sqrt(sum.diag.expected/(n*(n-sum.diag.expected)))
  z <- kappa/se.kappa.dist
  
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
  
  retval<-list(data.name   = "agreement contingency table",
               statistic   = z, 
               estimate    = c(kappa = kappa 
                               ,se.kappa = se.kappa
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Cohen's Kappa",
               conf.int    = c(ci.lower, ci.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "kappa"
  names(retval$parameter) <- "null hypothesis kappa"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}

