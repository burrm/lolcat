#Fix Power stuff - http://ucblibraries.summon.serialssolutions.com/#!/search?bookMark=ePnHCXMw42LgTQStzc4rAe_hSmGGHKsEbJkCW96WoNlHLmArGtiJBiYsIw5ozAOzjYExsHHMCSxMQDPQxeBxXIUC0EVhConQ0zkUgK04BaDJCpDRZKB4igJiL7sCtMYo5mZQcXMNcfbQLU2G9zjjoWMh8UnmZsDuoKUlaJMOUcoATIg_PQ
#
#

# sample.size.mean.t.twosample.independent <- function(effect.size
#                                                      ,se.est = 1
#                                                      ,alpha = .05
#                                                      ,beta = .1
#                                                      ,alternative = c("two.sided","less","greater")
#                                                      ,details = TRUE
#                                                      ,include.z = FALSE #Only valid with details = T
#                                                      ,power.from.actual = F #report parameter power instead of true power
# ) {
# 
#   size.1s <- sample.size.mean.t.onesample(effect.size = effect.size
#                                           ,se.est = se.est
#                                           ,alpha = alpha
#                                           ,beta = beta
#                                           ,alternative = alternative
#                                           ,details = T
#                                           ,include.z = F
#                                           ,power.from.actual = power.from.actual
#   )
#   
#   n <- size.1s$actual * 2
#   n.rounded <- ceiling(n)
#   
#   if (power.from.actual) {
#     
#   } else {
#     t_alpha <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df = n.rounded-2, lower.tail = F)
#     t_delta <- effect.size/(se.est/sqrt(n.rounded/2))
#     
#     beta <- pt(t_alpha
#                   , df = n.rounded - 2
#                   , lower.tail = T
#                   , ncp = t_delta
#     )
#   }
#   
#   
#   
#   if (details) {
#     as.data.frame(list(test="t"
#                        ,type = "two.sample"
#                        ,alternative = alternative[1]
#                        ,sample.size = n.rounded
#                        ,actual = n
#                        ,effect.size = effect.size
#                        ,se.est = se.est
#                        ,alpha = alpha
#                        ,conf.level = 1-alpha
#                        ,beta = beta
#                        ,power = 1- beta
#     ))
#     
#   }
#   else {
#     n.rounded
#   }
#   
# }








sample.size.mean.t.twosample.independent <- function(effect.size
                                                     ,variance.est.g1 = 1
                                                     ,variance.est.g2 = variance.est.g1
                                                     ,alpha = .05
                                                     ,beta = .1
                                                     ,alternative = c("two.sided","less","greater")
                                                     ,details = TRUE
                                                     ,power.from.actual = F #report parameter power instead of true power
) {

  se.est.g1 <- sqrt(variance.est.g1)
  se.est.g2 <- sqrt(variance.est.g2)
  
  #Use one-sample as starting point
  se.est <- sqrt((se.est.g1^2 + se.est.g2^2)/2)
  
  
  size.1s <- sample.size.mean.t.onesample(effect.size = effect.size
                                          ,variance.est = se.est
                                          ,alpha = alpha
                                          ,beta = beta
                                          ,alternative = alternative
                                          ,details = T
                                          #,include.z = F
                                          ,power.from.actual = power.from.actual
  )
  
  n <- ceiling(size.1s$actual)-1 


  #Build generating functions to help increment n until test beta <= beta
  
  ncp.fn <- function(n) {
    sqrt(n/2) * effect.size/se.est # d*sqrt(n1n2/(n1+n2))
  }
  
  beta.fn <- function(n, df) {
    t_alpha <- qt(ifelse(alternative[1] == "two.sided",alpha/2,alpha), df = df, lower.tail = F)
    
    pt(t_alpha
       , df = df
       , lower.tail = T
       , ncp = ncp.fn(n)
    )
  }

  df.fn <- function(n) { n-2 }
  
    
  df <- df.fn(2*n)
  beta.current <- beta.fn(n,df)

  
  while (beta.current > beta) {
    n <- n+1
    df <- df.fn(2*n)
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
                       ,var.est.g1 = variance.est.g1
                       ,var.est.g2 = variance.est.g2
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

#sample.size.mean.t.onesample(effect.size = 2, se.est = 5, include.z = F)
#sample.size.mean.t.twosample.independent(effect.size = 2, se.est.g1 = 1, se.est.g2 = 2)
