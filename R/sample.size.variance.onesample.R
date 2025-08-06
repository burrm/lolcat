sample.size.variance.onesample <- function(
  null.hypothesis.variance = 1
  ,alternative.hypothesis.variance = 2
  ,ratio = NA
  ,alpha = .05
  ,beta = .1
  ,alternative = c("two.sided","greater","less")
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
  
  
) {
  validate.htest.alternative(alternative = alternative)
  

  if (is.na(ratio)) {
    ratio <- alternative.hypothesis.variance/null.hypothesis.variance
  } else {
    null.hypothesis.variance <- 1
    alternative.hypothesis.variance <- ratio
  }
  n <- 2  

  if (ratio < .9991) {
    if (alternative[1] == "two.sided") {
      
    } else if (alternative[1] == "greater") {
      n <- NA  
    } else if (alternative[1] == "less") {
      
    } else {
      n <- NA
    }
    
  } else if (ratio > 1.0001) {
    
    if (alternative[1] == "two.sided") {
      
    } else if (alternative[1] == "greater") {
        
    } else if (alternative[1] == "less") {
      n <- NA
    } else {
      n <- NA
    }
    
  } else {
    n <- NA
    warning("Ratio close to 1 will not converge.")
  }
  
  current.beta <- NA
  
  if (!is.na(beta)) {
    if (!is.na(n)) {
    
      current.beta <- 1-power.variance.onesample(
        sample.size = n
        ,alternative.hypothesis.variance = alternative.hypothesis.variance
        ,null.hypothesis.variance = null.hypothesis.variance
        ,alpha = alpha
        ,alternative = alternative
        ,details = FALSE
      )

      #print(current.beta)
            
      while (current.beta > beta) {
        n <- n+1
      
        current.beta <- 1-power.variance.onesample(
          sample.size = n
          ,null.hypothesis.variance = null.hypothesis.variance
          ,alternative.hypothesis.variance = alternative.hypothesis.variance
          ,alpha = alpha
          ,alternative = alternative
          ,details = FALSE
        )
      }
    
    }
  } else {
    #beta is NA, use Wendy's calculation
    max_iter = 10000

    if (alternative[1] == "two.sided") {
      for(k in 2:max_iter) {
        n  <- k
        df <- n - 1
        
        # Compute Chi-square critical values
        chi2_lower <- qchisq(alpha / 2, df)
        chi2_upper <- qchisq(1 - alpha / 2, df)
        
        # Bounds of the variance confidence interval (expressed as ratios)
        lower_bound_ratio <- df / chi2_upper
        upper_bound_ratio <- df / chi2_lower
        
        # The interval [lower_bound_ratio, upper_bound_ratio] must be narrow enough 
        # so the true variance (ratio = 1) is within ± specified variance ratio
        # We check if lower_bound_ratio >= 1/variance_ratio AND upper_bound_ratio <= variance_ratio
        if((lower_bound_ratio >= 1/ratio) & (upper_bound_ratio <= ratio)) {
          break
        }
      }

      if (n == max_iter) {
        n <- NA
        warning("Did not find a suitable sample size within max_iter")
      }
    } else if (alternative[1] == "less") {
      # ratio here is the fraction of true σ you’ll tolerate (e.g. 0.90 for –10%)
      for (k in 2:max_iter) {
        n         <- k
        df        <- n - 1
        chi2_up   <- qchisq(1 - alpha, df)   # upper-tail critical value
        lower_bd  <- df / chi2_up              # lower CI limit on σ²/s²
        
        if (lower_bd >= ratio) {
          break
        }
      }

      if (n == max_iter) {
        n <- NA
        warning("No suitable n found up to ", max_iter)
      }


    } else if (alternative[1] == "greater") {
      # ratio here is the multiple of true σ you’ll tolerate (e.g. 1.10 for +10%)
      for (k in 2:max_iter) {
        n         <- k
        df        <- n - 1
        chi2_low  <- qchisq(alpha, df)       # lower-tail critical value
        upper_bd  <- df / chi2_low             # upper CI limit on σ²/s²
        
        if (upper_bd <= ratio) {
          break
        }
      }

      if (n == max_iter) {
        n <- NA
        warning("No suitable n found up to ", max_iter)
      }
    }

  }

  if (power.from.actual) {
    
  } else {
    
    beta <- current.beta
  }
  
  if (details) {
    as.data.frame(list(test="chi-square"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,df = n - 1
                       ,ratio = ratio
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
  }
  else {
    n
  }
    
}

#sample.size.variance.onesample(alternative.hypothesis.variance = 2, null.hypothesis.variance = 1)
#qchisq(.025, 10, lower.tail = F)


# alpha <- 0.05        # 95% confidence
# ratio <- 1.1      # Allow observed SD within ±10% of true SD
# #required_n <- find_sample_size_variance(alpha, sd_ratio)
# required_n <- sample.size.variance.onesample(alpha = alpha, beta = NA, ratio = ratio)
# required_n
#
# test for "less"
# alpha  <- 0.05
# ratio  <- 0.90
# #find_sample_size_lower(alpha, sd_ratio_lo)
# sample.size.variance.onesample(alpha = alpha, beta = NA, ratio = ratio, alternative = "less")
#
# alpha  <- 0.05
# ratio  <- 1.1
# #find_sample_size_upper(alpha, sd_ratio_up)
# sample.size.variance.onesample(alpha = alpha, beta = NA, ratio = ratio, alternative = "greater")

