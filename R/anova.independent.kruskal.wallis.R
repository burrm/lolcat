anova.independent.kruskal.wallis <- function(
  fx 
  ,data = NULL 
  ,tie.correct = T #Tie correction
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  #iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  
  cell.codes <- compute.group.cell.codes(fx =fx, data = data)
  case.ranks <- rank(data[[response]])
  
  d <- data.frame(dv = case.ranks, cell = cell.codes)
  
  summary.out <- summary.impl(dv~cell, data = d, stat.n = T, stat.sum = T)
  
  N  <- sum(summary.out$n)
  H  <- (12/(N*(N+1)))*sum(summary.out$sum^2/summary.out$n) -3*(N+1)
  df <- nrow(summary.out)-1
  
  C <- 1
  
  if (tie.correct) {
    zz <- as.vector(table(case.ranks))
    tie.sum <- sum(sapply(zz, FUN = function(x) {x^3 - x}))
    
    tie.sum <- tie.sum/(N^3-N)
    
    C <- C-tie.sum
    
    H <- H/C
  }
  
  
  
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(H,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(H,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(H,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "ranks of data and cell codes",
               statistic   = c(H = H), 
               estimate    = c(
                 df = df
                 ,total.sample.size = N
                 ,number.of.groups = df+1
                 ,tie.correct.C = C
               ),
               parameter   = df,
               p.value     = p.value,
               null.value  = df,
               alternative = alternative[1],
               method      = "Kruskal-Wallis One-Way Analysis of Variance By Ranks",
               conf.int    = c(NA, NA)
  )
  
  names(retval$null.value) <- "Kruskal-Wallis H"
  names(retval$parameter) <- "null hypothesis Kruskal-Wallis H"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}