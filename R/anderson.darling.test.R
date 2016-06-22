anderson.darling.test <-
  function(x
           #,conf.level = .95
           )
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs the Anderson-Darling normality test
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type
    #       character.
    #   title - a character string which allows for a project title.
    
    # Source:
    #   Package: nortest
    #   Title: Tests for Normality
    #   Version: 1.0
    #   Author: Juergen Gross
    #   Description: 5 omnibus tests for the composite hypothesis of normality
    #   Maintainer: Juergen Gross <gross@statistik.uni-dortmund.de>
    #   License: GPL version 2 or newer
    
    # Thanks:
    #   to Spencer Grave for contributions.
    
    # FUNCTION:
    
    dname <- "input data"    
    x <- na.omit(x)
    
    # Test:
    x = sort(x)
    n = length(x)
    if (n < 2) stop("sample size must be greater than 1")
    var.x <- var(x)
    
    if(var.x > 0){
      p = pnorm((x - mean(x))/sqrt(var.x))
      h = (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
      ### DW modified:
      h = h[is.finite(h)]
      n = length(h)
      ### Continue:
      A = -n - mean(h)
      AA = (1 + 0.75/n + 2.25/n^2) * A
      
      if (AA < 0.2) {
        PVAL = 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
      } else if (AA < 0.34) {
        PVAL = 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
      } else if (AA < 0.6) {
        PVAL = exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
      } else {
        PVAL = exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
      }
      # Result:
      if (PVAL > 1) {
        PVAL = 1 # was NA, suggested by Spencer Graves to modify
        W = NA
      }
    } else {
      A <- Inf
      PVAL <- 0
    }
    names(PVAL) = ""
    
    retval<-list(data.name   = dname,
                 statistic   = AA, 
                 estimate    = c(AA,A),
                 parameter   = 0 ,
                 p.value     = rmnames(PVAL),
                 null.value  = 0,
                 alternative = "two.sided",
                 method      = "Anderson - Darling Normality Test"#,
#                 conf.int    = c(NA,NA)
    )
    
    names(retval$estimate) <- c("AA", "A")
    names(retval$statistic) <- "AA"
    names(retval$null.value) <- "skewness and kurtosis"
    names(retval$parameter) <- "null hypothesis skewness and kurtosis"
    #attr(retval$conf.int, "conf.level")  <- conf.level
    
    class(retval)<-"htest"
    retval
    
  }
