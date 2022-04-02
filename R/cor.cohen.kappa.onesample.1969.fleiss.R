#' Cohen's Kappa (Fleiss 1969)  
#' 
#' Calculate Cohen's Kappa based on Fleiss' 1969 paper.
#'
#' @param observed.frequencies A matrix of values to test.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @aliases cor.cohen.kappa.onesample
#' 
#' @return The results of the statistical test.

cor.cohen.kappa.onesample.1969.fleiss <- function(
  observed.frequencies #matrix
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
    
  n <- sum(observed.frequencies)
  n.agree <- sum(diag(observed.frequencies))
  n.disagree <- n - n.agree
  
  i.set <- 1:nrow(observed.frequencies)
  j.set <- 1:ncol(observed.frequencies)

  p_ij <- observed.frequencies/n 
  p_i. <- rowSums(p_ij) #eq [1]
  p_.j <- colSums(p_ij) #eq [2]

  p_o <- sum(diag(p_ij)) #eq [10]
  p_c <- sum(
    #sum over rows
    sapply(i.set, FUN = function(i) {
      sum(
        #sum over cols
        sapply(j.set, FUN = function(j) {
           if( i == j) {
             p_i.[i] * p_.j[j]
           } else {
             0
           }
        })
      )
    })
  ) # eq [11]

  kappa <- (p_o - p_c) / (1-p_c) #eq [12]

  var_kappa <- (1/(n*(1-p_c)^4)) * (
    #sum over diagonal
    sum(sapply(i.set,FUN = function(i) {
      p_ij[i,i]*((1-p_c)-(p_i.[i]+p_.j[i])*(1-p_o))^2
    })) +
    #sum over rows
    ((1-p_o)^2)*sum(sapply(i.set,FUN = function(i) {
      #sum over cols
      sum(sapply(j.set, FUN = function(j) {
        #non diagonal elements only
        if (i != j) {
          p_ij[i,j]*(p_.j[i]+p_i.[j])^2
        } else {
          0
        }
      }))
    }))
    - (p_o*p_c - 2*p_c + p_o)^2
  ) # eq [13]

  


  var_0_kappa <- (1/(n*(1-p_c)^2)) * (
    #sum over diagonal
    sum(sapply(i.set,FUN = function(i) {
      p_i.[i]*p_.j[i]*(1-(p_.j[i]+p_i.[i]))^2
    })) +
    #sum over rows
    sum(sapply(i.set,FUN = function(i) {
      #sum over cols
      sum(sapply(j.set, FUN = function(j) {
        #non diagonal elements only
        if (i != j) {
          p_i.[i]*p_.j[j]*(p_.j[i]+p_i.[j])^2
        } else {
          0
        }
      }))
    })) - p_c^2
  ) #eq [14]

  # Kappa Max 
  p.om <- sum(pmin(p_i., p_.j))
  k_max <- (p.om - p_c) / (1 - p_c)

  se_kappa   <- sqrt(var_kappa)
  se_0_kappa <- sqrt(var_0_kappa)

  z <- kappa/se_0_kappa
 
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ci.add  <- cv*se_kappa
  
  ci.lower <- max(-1,kappa-ci.add)
  ci.upper <- min(1,kappa+ci.add)
  
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
                               ,var_kappa = var_kappa
                               ,se_kappa = se_kappa
                               ,var_0_kappa = var_0_kappa
                               ,se_0_kappa = se_0_kappa
                               ,kappa.max = k_max
                               ,p_o = p_o
                               ,p_c = p_c
                               ,n = n
                               ,n.agree = n.agree
                               ,n.disagree = n.disagree
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Cohen's Kappa (Fleiss, et. al. 1969 Confidence Intervals)",
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


#' @rdname cor.cohen.kappa.onesample.1969.fleiss
cor.cohen.kappa.onesample <- cor.cohen.kappa.onesample.1969.fleiss
