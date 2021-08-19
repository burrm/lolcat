#' Two-Sample Test for Differences in Cohen's Kappa   
#' 
#' Calculate test to determine if two calculations of Cohen's kappa differ statistically.
#'
#' @param kappa.g1 Scalar - Kappa from the first calculation.
#' @param se.kappa.g1 Scalar - Standard error from first calculation of kappa.
#' @param kappa.g2 Scalar - Kappa from the second calculation.
#' @param se.kappa.g2 Scalar - Standard error from second calculation of kappa.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return The results of the statistical test.

cor.cohen.kappa.twosample.independent.simple <- function(
  kappa.g1
  ,se.kappa.g1
  ,kappa.g2
  ,se.kappa.g2
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  z <- (kappa.g1 - kappa.g2)/sqrt(se.kappa.g1^2  + se.kappa.g2^2)
  
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
  
  retval<-list(data.name   = "kappa values",
               statistic   = z, 
               estimate    = c(kappa.g1 = kappa.g1 
                               ,se.kappa.g1 = se.kappa.g1
                               ,kappa.g2 = kappa.g2 
                               ,se.kappa.g2 = se.kappa.g2
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Two-Sample Independent Cohen's Kappa",
               conf.int    = c(NA, NA)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "kappa difference"
  names(retval$parameter) <- "null hypothesis kappa difference"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}