#' Kendall's Coefficient of Concordance 
#' 
#' Calculate Kendall's Coefficient of Concordance (sometimes called Kendall's W).
#' 
#' @param x Matrix - matrix of ratings 
#' @param raters.in.column.variable Logical - The raters are expected to be in the columns, if this is not the case, then set to FALSE.
#' @param call.rank Logical - if true, rank() is called. If FALSE, then values are used as-is 
#' @param tie.correct Logical - if true, a correction for ties is applied to the calculation. 
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return Hypothesis test result showing results of test.
cor.kendall.coefficient.concordance <- function(
  x #A matrix of ratings
  ,raters.in.column.variable = T
  ,call.rank = T
  ,tie.correct = T
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  if (raters.in.column.variable) {
    
  } else {
    x <- t(x)
  }
  
  if (call.rank) {
    for (i in 1:ncol(x)) {
      x[,i] <- rank(x[,i])
    }
  }
  
  #TODO - develop structure for tie information
  tie.sum <- 0
  
  if (tie.correct) {
    tie.sum <- sum(apply(x,2, FUN = function(x) {
      zz <- as.vector(table(x))
      
      sum(sapply(zz, FUN = function(x) {x^3 - x}))
    }))
  }
  
  x <- t(x) #columns are now subjects
  
  n <- ncol(x)
  m <- nrow(x)
  
  #Calculate tie correction
  tie.correction <- m * tie.sum
  
  
  sum.ranks <- apply(x,2,sum)
  
  sum.ranks.sq <- sum.ranks^2
  
  
  W_T <- sum(sum.ranks)
  W_U <- sum(sum.ranks.sq)
  
  S <- (n*W_U - W_T^2)/n
  
  if (tie.correct) {
    
  } else {
    tie.correction <- 0
  }
  
  W <- (12 * W_U - 3*m^2 * n * (n+1)^2)/(m^2 * n * (n^2 - 1) - tie.correction)
  
  chi.square <- m*(n-1)*W
  df <- n-1
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square, df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square, df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square, df,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  
  retval<-list(data.name   = "ranks of n subjects by m raters",
               statistic   = c(chi.square = chi.square), 
               estimate    = c(W = W
                               ,count.subjects = n
                               ,count.raters = m
               ),
               parameter   = n-1,
               p.value     = p.value,
               null.value  = n-1,
               alternative = alternative[1],
               method      = "Kendall's Coefficient of Concordance (W)",
               conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "chi-square"
  names(retval$parameter) <- "null hypothesis chi-square"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}