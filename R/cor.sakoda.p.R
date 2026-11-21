#' Sakoda's p* Coefficient 
#' 
#' Calculate Sakoda's p* for a contingency table. 
#' 
#' @param x Matrix - contingency table 
#'
#' @return Hypothesis test result showing results of test.
cor.sakoda.p <- function(
  x
) {
  
  phi.result <- cor.pearson.phi(x)
  phi <- rmnames(phi.result$statistic)
  q <- min(nrow(x),ncol(x))
  
  p <- sqrt(q * phi^2 / ((q-1)(1+phi^2)))
  
  
  retval<-list(data.name   = "data",
               statistic   = c(p = p), 
               estimate    = c(phi = rmnames(phi.result$statistic), 
                               phi.p = rmnames(phi.result$p.value), 
                               phi$estimate
                             ),
               parameter   = 0 ,
               p.value     = phi.result$p.value,
               null.value  = 0,
               alternative = "two.sided",
               method      = "Sakoda p*, Pearson Phi, and Pearson Chi-Squared"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "p"
  names(retval$parameter) <- "null hypothesis p"
  
  class(retval)<-"htest"
  retval
}