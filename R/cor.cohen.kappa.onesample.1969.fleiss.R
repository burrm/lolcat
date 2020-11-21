cor.cohen.kappa.onesample.1969.fleiss <- function(
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
  
  # Kappa
  kappa    <- (sum.diag.observed - sum.diag.expected)/(n - sum.diag.expected)

  # Kappa Max

  prop.observed <- observed.frequencies / n

  prop.row.sums <- rowSums(prop.observed)
  prop.col.sums <- colSums(prop.observed)
  prop.diag     <- diag(prop.observed)
  
  p.o <- sum(diag(prop.observed))
  p.c <- sum(prop.row.sums * prop.col.sums) 
  p.om <- sum(pmin(prop.row.sums, prop.col.sums))
  k.max <- (p.om - p.c) / (1 - p.c)

  #SE Kappa and CI

  #no association SE
  #se.kappa <- sqrt(p.c+p.c*p.c-(sum((prop.row.sums + prop.col.sums)*prop.row.sums*prop.col.sums)))/(1-p.c)/sqrt(n)
  
  #association SE
  diag.1     <- sum(prop.diag*(1-(prop.row.sums+prop.col.sums)*(1-kappa))^2)

  #print(paste0("diag 1 = ", diag.1))

  off.diag.1 <- (1-kappa)^2*sum(
    sapply(1:length(prop.row.sums),
      FUN = function(row.i) {
        sum(
          sapply(1:length(prop.col.sums), FUN= function(col.i) {
            if (row.i == col.i) {
              0
            } else {
              prop.observed[row.i, col.i]*(prop.row.sums[row.i] + prop.col.sums[col.i])^2
            }
          })
        )
      }
    )
  )

  #print(paste0("off diag 1 = ", off.diag.1))

  p.star.c <- (kappa - p.c *(1-kappa))^2

  #print(paste0("p star c = ", p.star.c))

  se.kappa <- sqrt(diag.1+off.diag.1-p.star.c)/(1-p.c)/sqrt(n)

  z <- kappa/se.kappa
 
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ci.add  <- cv*se.kappa
  
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
               method      = "Cohen's Kappa (Fleiss, et. al 1969 Confidence Intervals)",
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



cor.cohen.kappa.onesample <- cor.cohen.kappa.onesample.1969.fleiss
