#' Cohen's Kappa (1960)  
#' 
#' Calculate Cohen's Kappa.
#'
#' @param observed.frequencies A matrix of values to test.
#' @param expected.frequencies A matrix of values to compare with observed.frequencies.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return The results of the statistical test.

cor.cohen.kappa.onesample.1960.cohen <- function(
  observed.frequencies #matrix
  ,expected.frequencies = chi.square.2d.expected.frequencies(observed.frequencies)
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
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
  
  prop.observed <- observed.frequencies / n

  prop.row.sums <- rowSums(prop.observed)
  prop.col.sums <- colSums(prop.observed)
  
  p.o <- sum(diag(prop.observed))
  p.c <- sum(prop.row.sums * prop.col.sums) 
  p.om <- sum(pmin(prop.row.sums, prop.col.sums))
  k.max <- (p.om - p.c) / (1 - p.c)
  

  retval<-list(data.name   = "agreement contingency table",
               statistic   = z, 
               estimate    = c(kappa = kappa 
                               ,se.kappa = se.kappa
                               ,kappa.max = k.max
                               ,p.o = p.o
                               ,p.c = p.c
                               ,n.agree = sum.diag.observed
                               ,n.disagree = (n-sum.diag.observed)
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Cohen's Kappa (Cohen 1960 Confidence Intervals)",
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

