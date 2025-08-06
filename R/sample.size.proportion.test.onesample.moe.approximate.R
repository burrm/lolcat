#' Calculate Sample Size Based on Margin of Error - Proportion  
#' 
#' Calculate sample size based on margin of error for proportions. 
#'
#' @param proportion Lower specification limit (if applicable)
#' @param margin.of.error Margin of error (ex. .2 means ±10% margin) 
#' @param alpha Target type I error rate
#' @param alternative Design test type (1 or 2 tailed, if 1 tail, which tail?)
#' @param details Return detailed information, if false, returns n only. 
#' @param max.sample.size Return NA if calculation fails to converge by max.sample.size.
#'
#' @return if details is set to TRUE, a data frame with computed measures. If details is set to FALSE, then a single value for computed sample size, n. 

# Code submitted by Wendy Martin (CU Boulder) and adapted to package conventions

sample.size.proportion.test.onesample.moe.approximate <- function(proportion = .5
                                                              ,margin.of.error = .2
                                                              ,alpha = .05
                                                              ,alternative = c("two.sided","less","greater")
                                                              ,details = TRUE
                                                              ,max.sample.size = 10000
) {
  validate.htest.alternative(alternative = alternative)
  
  n <- 1

  if (alternative[1] == "two.sided") {
    for (k in 1:max.sample.size) {
      n <- k
      # conservative counts for each tail
      x_lo <- floor(n * proportion)
      x_hi <- ceiling(n * proportion)
      
      # lower limit with x_lo
      PL <- if (x_lo == 0) {
        0
      } else {
        qbeta(alpha/2, x_lo, n - x_lo + 1)
      }
      
      # upper limit with x_hi
      PU <- if (x_hi >= n) {
        1
      } else {
        qbeta(1 - alpha/2, x_hi + 1, n - x_hi)
      }
      
      # check both margins
      if ((proportion - PL) <= margin.of.error && (PU - proportion) <= margin.of.error) {
        break
      }
    }
  
    if (n == max.sample.size) {
      warning("No n ≤ ", max.sample.size, " meets the two-sided exact CI width requirement")
      n <- NA
    }

  } else if (alternative[1] == "less") {
    for (k in 1:max.sample.size) {
      n <- k
      x <- floor(n * proportion)  
      PL <- if (x == 0) {
        0
      } else {
        qbeta(alpha, x, n - x + 1)
      }
      if ((proportion - PL) <= margin.of.error) {
        break
      }
    }

    if (n == max.sample.size) {
          warning("No n ≤ ", max.sample.size, " meets one-sided lower requirement")
          n <- NA
    }

  } else if (alternative[1] == "greater") {
    for (k in 1:max.sample.size) {
      n <- k
      x  <- ceiling(n * proportion)
      PU <- if (x == n) {
        1
      } else {
        qbeta(1 - alpha, x + 1, n - x)
      }
      if ((PU - proportion) <= margin.of.error) {
        return(n)
      }
    }

    if (n == max.sample.size) {
          warning("No n ≤ ", max.sample.size, " meets one-sided upper requirement")
          n <- NA
    }

  }

  
  if (details) {
    as.data.frame(list(test="proportion"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,actual = n
                       ,proportion = proportion
                       ,margin.of.error = margin.of.error
                       ,alpha = alpha
                       ,conf.level = 1-alpha
    ))
    
  }
  else {
    n
  }
}


