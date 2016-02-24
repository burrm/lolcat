sample.size.mean.t.twosample.dependent.dbar <- function(effect.size
                                                   ,se.est.d = 1
                                                   ,alpha = .05
                                                   ,beta = .1
                                                   ,alternative = c("two-sided","less","greater")
                                                   ,details = TRUE
                                                   ,power.from.actual = F #report parameter power instead of true power
) {
  
  se.est <- se.est.d
  
  
  n <- 2 
  
  
  #Build generating functions to help increment n until test beta <= beta
  
  ncp.fn <- function(n) {
     effect.size/(se.est/sqrt(n)) 
  }
  
  beta.fn <- function(n,df) {
    t_alpha <- qt(ifelse(alternative[1] == "two-sided",alpha/2,alpha), df = df, lower.tail = F)
    
    pt(t_alpha
       , df = df
       , lower.tail = T
       , ncp = ncp.fn(n)
    )
  }
  
  df.fn <- function(n) { n-1 }
  
  
  df <- df.fn(n)
  beta.current <- beta.fn(n,df)
  
  
  while (beta.current > beta) {
    n <- n+1
    df <- df.fn(n)
    beta.current <- beta.fn(n,df)

}
  
  
  
  
  
  
  
  if (power.from.actual) {
    
  } else {
    beta <- beta.current
  }
  
  
  
  if (details) {
    as.data.frame(list(test="t"
                       ,type = "two.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,df = df
                       ,effect.size = effect.size
                       ,se.est = se.est
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


#sample.size.mean.t.twosample.dependent(effect.size = 2, se.est.d = 1)
