explore.johnson <- function(x
                            ,z.min = .05
                            ,z.max = 1
                            ,step  = .01
                            
                            ,quantile.type = 8
                            
                            #Tests to return
                            ,stat.ad.test   = F
                            ,stat.sw.test   = F
                            ,stat.skew.test = T
                            ,stat.kurt.test = T
                            ,stat.pois.dist.test = F
                            ,stat.sw.exp.test = F
                            
) {
  opt.orig <- rmnames(unlist(options("stringsAsFactors")))
  orig.warnings <- unlist(options("warn"))
  options(warn = -1)
  
  
  options(stringsAsFactors = F)
  
  x <- na.omit(x)
  
  n <- length(x)
  
  sorted_x <- sort(x)
  
  all.z <- seq(z.min
               ,z.max
               ,step)
  
  
  ret <- data.frame(z             = numeric(0)
                    ,transform    = character(0)
                    ,mn.over.p.sq = numeric(0)
                    ,gamma        = numeric(0)
                    ,eta          = numeric(0)
                    ,epsilon      = numeric(0)
                    ,lambda       = numeric(0)
                    ,adtest.AA    = numeric(0)
                    ,adtest.p     = numeric(0)
                    
                    ,swtest.W     = numeric(0)
                    ,swtest.p     = numeric(0)
                    
                    ,g3.skewness  = numeric(0)
                    ,g3test.z     = numeric(0)
                    ,g3test.p     = numeric(0)
                    
                    ,g4.kurtosis  = numeric(0)
                    ,g4test.z     = numeric(0)
                    ,g4test.p     = numeric(0)
                    
                    ,pois.test.chi.square = numeric(0)
                    ,pois.test.p          = numeric(0)
                    
                    ,sw.exp.test.W = numeric(0)
                    ,sw.exp.test.p = numeric(0)
                    
  )
  
  for (z in all.z) {
    z.m3 <- -3*z
    z.m1 <- -1*z
    z.1  <- z
    z.3  <- 3*z
    
    p.z.m3 <- pnorm(z.m3)
    p.z.m1 <- pnorm(z.m1)
    p.z.1  <- pnorm(z.1)
    p.z.3  <- pnorm(z.3)
    
    x.zeta.ary <- rmnames(quantile(sorted_x, probs = c(p.z.m3,p.z.m1,p.z.1,p.z.3), type = quantile.type))
    
    x.zeta.m3 <- x.zeta.ary[1]
    x.zeta.m1 <- x.zeta.ary[2]
    x.zeta.1  <- x.zeta.ary[3]
    x.zeta.3  <- x.zeta.ary[4]
    
    est.m <- x.zeta.3 - x.zeta.1
    est.n <- x.zeta.m1 - x.zeta.m3
    est.p <- x.zeta.1 - x.zeta.m1
    

    this.iter.su <- list(
      z             = z
      ,transform    = "su"
      ,mn.over.p.sq = est.m*est.n/(est.p^2)
      ,gamma        = NA
      ,eta          = NA
      ,epsilon      = NA
      ,lambda       = NA
      
      ,adtest.AA    = NA
      ,adtest.p     = NA
      
      ,swtest.W     = NA
      ,swtest.p     = NA
      
      ,g3.skewness  = NA
      ,g3test.z     = NA
      ,g3test.p     = NA
      
      ,g4.kurtosis  = NA
      ,g4test.z     = NA
      ,g4test.p     = NA
      
      ,pois.test.chi.square = NA
      ,pois.test.p          = NA
      
      ,sw.exp.test.W = NA
      ,sw.exp.test.p = NA
    )
    
    this.iter.isu <- this.iter.su
    this.iter.isu$transform <- "inv_su"
    
    this.iter.sb <- this.iter.su
    this.iter.sb$transform <- "sb"
    
    this.iter.isb <- this.iter.su
    this.iter.isb$transform <- "inv_sb"
    
    
    this.iter.sl <- this.iter.su
    this.iter.sl$transform <- "sl"

    this.iter.isl <- this.iter.su
    this.iter.isl$transform <- "inv_sl"
    
    
    #Johnson SU Parameters
    
    p1 <- parameters.johnson.su(z
                                ,quantile.neg.3z = x.zeta.m3
                                ,quantile.neg.z = x.zeta.m1
                                ,quantile.z = x.zeta.1
                                ,quantile.3z = x.zeta.3)
    
    this.iter.su$eta     <- p1$eta
    this.iter.su$gamma   <- p1$gamma
    this.iter.su$lambda  <- p1$lambda
    this.iter.su$epsilon <- p1$epsilon

    this.iter.isu$eta     <- p1$eta
    this.iter.isu$gamma   <- p1$gamma
    this.iter.isu$lambda  <- p1$lambda
    this.iter.isu$epsilon <- p1$epsilon
    
        
    rm(p1)
    
    if (all(is.finite(c(this.iter.su$eta, this.iter.su$gamma, this.iter.su$lambda, this.iter.su$epsilon)))) {
      trans_x <- transform.johnson.su(x 
                                      ,gamma   = this.iter.su$gamma
                                      ,eta     = this.iter.su$eta
                                      ,lambda  = this.iter.su$lambda
                                      ,epsilon = this.iter.su$epsilon
                                      )
      
      t1 <- process.shape.tests(x = trans_x
        ,stat.ad.test   = stat.ad.test
        ,stat.sw.test   = stat.sw.test
        ,stat.skew.test = stat.skew.test
        ,stat.kurt.test = stat.kurt.test
        ,stat.pois.dist.test = stat.pois.dist.test
        ,stat.sw.exp.test = stat.sw.exp.test
      )
      
      for (i in names(t1)) {
        this.iter.su[[i]] <- t1[[i]]
      }
      
      rm(t1, trans_x)
      
      
      # trans_x <- transform.johnson.su(x 
      #                                 ,gamma   = this.iter.isu$gamma
      #                                 ,eta     = this.iter.isu$eta
      #                                 ,lambda  = this.iter.isu$lambda
      #                                 ,epsilon = this.iter.isu$epsilon
      #                                 ,inverse = T
      # )
      # 
      # t1 <- process.shape.tests(x = trans_x
      #                           ,stat.ad.test   = stat.ad.test
      #                           ,stat.sw.test   = stat.sw.test
      #                           ,stat.skew.test = stat.skew.test
      #                           ,stat.kurt.test = stat.kurt.test
      #                           ,stat.pois.dist.test = stat.pois.dist.test
      #                           ,stat.sw.exp.test = stat.sw.exp.test
      # )
      # 
      # for (i in names(t1)) {
      #   this.iter.isu[[i]] <- t1[[i]]
      # }
      # 
      # rm(t1, trans_x)
      # 
      
    }
    
    
    #Johnson SB parameter estimates 
    
    p1 <- parameters.johnson.sb(z
                                ,quantile.neg.3z = x.zeta.m3
                                ,quantile.neg.z = x.zeta.m1
                                ,quantile.z = x.zeta.1
                                ,quantile.3z = x.zeta.3)
    
    this.iter.sb$eta     <- p1$eta
    this.iter.sb$gamma   <- p1$gamma
    this.iter.sb$lambda  <- p1$lambda
    this.iter.sb$epsilon <- p1$epsilon
    
    rm(p1)

    
    if (all(is.finite(c(this.iter.sb$eta, this.iter.sb$gamma, this.iter.sb$lambda, this.iter.sb$epsilon)))) {
      trans_x <- transform.johnson.sb(x 
                                      ,gamma   = this.iter.sb$gamma
                                      ,eta     = this.iter.sb$eta
                                      ,lambda  = this.iter.sb$lambda
                                      ,epsilon = this.iter.sb$epsilon
      )
      
      t1 <- process.shape.tests(x = trans_x
                                ,stat.ad.test   = stat.ad.test
                                ,stat.sw.test   = stat.sw.test
                                ,stat.skew.test = stat.skew.test
                                ,stat.kurt.test = stat.kurt.test
                                ,stat.pois.dist.test = stat.pois.dist.test
                                ,stat.sw.exp.test = stat.sw.exp.test
      )
      
      for (i in names(t1)) {
        this.iter.sb[[i]] <- t1[[i]]
      }
      
      rm(t1, trans_x)
      
      # 
      # trans_x <- transform.johnson.sb(x 
      #                                 ,gamma   = this.iter.isb$gamma
      #                                 ,eta     = this.iter.isb$eta
      #                                 ,lambda  = this.iter.isb$lambda
      #                                 ,epsilon = this.iter.isb$epsilon
      #                                 ,inverse = T
      # )
      # 
      # t1 <- process.shape.tests(x = trans_x
      #                           ,stat.ad.test   = stat.ad.test
      #                           ,stat.sw.test   = stat.sw.test
      #                           ,stat.skew.test = stat.skew.test
      #                           ,stat.kurt.test = stat.kurt.test
      #                           ,stat.pois.dist.test = stat.pois.dist.test
      #                           ,stat.sw.exp.test = stat.sw.exp.test
      # )
      # 
      # for (i in names(t1)) {
      #   this.iter.isb[[i]] <- t1[[i]]
      # }
      # 
      # rm(t1, trans_x)
      # 
    }
    
    
    
    
    
        
    #Johnson SL parameter estimates 
    
    p1 <- parameters.johnson.sl(z
                                ,quantile.neg.3z = x.zeta.m3
                                ,quantile.neg.z = x.zeta.m1
                                ,quantile.z = x.zeta.1
                                ,quantile.3z = x.zeta.3)
    
    this.iter.sl$eta     <- p1$eta
    this.iter.sl$gamma   <- p1$gamma
    this.iter.sl$lambda  <- p1$lambda
    this.iter.sl$epsilon <- p1$epsilon

    this.iter.isl$eta     <- p1$eta
    this.iter.isl$gamma   <- p1$gamma
    this.iter.isl$lambda  <- p1$lambda
    this.iter.isl$epsilon <- p1$epsilon
    
        
    rm(p1)
    
    
    if (all(is.finite(c(this.iter.sl$eta, this.iter.sl$gamma, this.iter.sl$lambda, this.iter.sl$epsilon)))) {
      trans_x <- transform.johnson.sl(x 
                                      ,gamma   = this.iter.sl$gamma
                                      ,eta     = this.iter.sl$eta
                                      ,lambda  = this.iter.sl$lambda
                                      ,epsilon = this.iter.sl$epsilon
      )
      
      t1 <- process.shape.tests(x = trans_x
                                ,stat.ad.test   = stat.ad.test
                                ,stat.sw.test   = stat.sw.test
                                ,stat.skew.test = stat.skew.test
                                ,stat.kurt.test = stat.kurt.test
                                ,stat.pois.dist.test = stat.pois.dist.test
                                ,stat.sw.exp.test = stat.sw.exp.test
      )
      
      for (i in names(t1)) {
        this.iter.sl[[i]] <- t1[[i]]
      }
      
      rm(t1, trans_x)
      
      # trans_x <- transform.johnson.sl(x 
      #                                 ,gamma   = this.iter.isl$gamma
      #                                 ,eta     = this.iter.isl$eta
      #                                 ,lambda  = this.iter.isl$lambda
      #                                 ,epsilon = this.iter.isl$epsilon
      #                                 ,inverse = T
      # )
      # 
      # t1 <- process.shape.tests(x = trans_x
      #                           ,stat.ad.test   = stat.ad.test
      #                           ,stat.sw.test   = stat.sw.test
      #                           ,stat.skew.test = stat.skew.test
      #                           ,stat.kurt.test = stat.kurt.test
      #                           ,stat.pois.dist.test = stat.pois.dist.test
      #                           ,stat.sw.exp.test = stat.sw.exp.test
      # )
      # 
      # for (i in names(t1)) {
      #   this.iter.isl[[i]] <- t1[[i]]
      # }
      # 
      # rm(t1, trans_x)
    }
    
    ret <- rbind(ret, this.iter.su, this.iter.sb, this.iter.sl)
    
  }
  
  
  
  
  
  options(stringsAsFactors = opt.orig)
  options(warn = orig.warnings)
  
  
  ret
}










# explore.johnson <- function(x
#                             ,z.min = .05
#                             ,z.max = .95
#                             ,step  = .01
#                             
#                             #Tests to return
#                             ,stat.ad.test   = F
#                             ,stat.sw.test   = F
#                             ,stat.skew.test = T
#                             ,stat.kurt.test = T
#                             ,stat.pois.dist.test = F
#                             ,stat.sw.exp.test = F
#                             
# ) {
#   opt.orig <- rmnames(unlist(options("stringsAsFactors")))
#   
#   options(stringsAsFactors = F)
#   
#   x <- na.omit(x)
#   
#   n <- length(x)
#   
#   sorted_x <- sort(x)
#   
#   all.z <- seq(z.min
#                ,z.max
#                ,step)
#   
#   
#   ret <- data.frame(z             = numeric(0)
#                     ,transform    = character(0)
#                     ,mn.over.p.sq = numeric(0)
#                     ,gamma        = numeric(0)
#                     ,eta          = numeric(0)
#                     ,epsilon      = numeric(0)
#                     ,lambda       = numeric(0)
#                     ,adtest.AA    = numeric(0)
#                     ,adtest.p     = numeric(0)
#                     
#                     ,swtest.W     = numeric(0)
#                     ,swtest.p     = numeric(0)
#                     
#                     ,g3.skewness  = numeric(0)
#                     ,g3test.z     = numeric(0)
#                     ,g3test.p     = numeric(0)
#                     
#                     ,g4.kurtosis  = numeric(0)
#                     ,g4test.z     = numeric(0)
#                     ,g4test.p     = numeric(0)
#                     
#                     ,pois.test.chi.square = numeric(0)
#                     ,pois.test.p          = numeric(0)
#                     
#                     ,sw.exp.test.W = numeric(0)
#                     ,sw.exp.test.p = numeric(0)
#                     
#   )
#   
#   for (z in all.z) {
#     z.m3 <- -3*z
#     z.m1 <- -1*z
#     z.1  <- z
#     z.3  <- 3*z
#     
#     p.z.m3 <- pnorm(z.m3)
#     p.z.m1 <- pnorm(z.m1)
#     p.z.1  <- pnorm(z.1)
#     p.z.3  <- pnorm(z.3)
#     
#     print(paste("pm3",p.z.m3,"pm1",p.z.m1,"p1",p.z.1, "p3", p.z.3))
#     
#     
#     zeta.m3 <- n*p.z.m3 + 1/2
#     zeta.m1 <- n*p.z.m1 + 1/2
#     zeta.1  <- n*p.z.1 + 1/2
#     zeta.3  <- n*p.z.3 + 1/2
#     
#     print(paste("im3",zeta.m3,"im1",zeta.m1,"i1",zeta.1, "i3", zeta.3))
#     
#     
#     #local fitting variables (interpolation of percentiles)
#     
#     zeta.ary <- c(zeta.m3, zeta.m1, zeta.1, zeta.3)
#     
#     x.zeta.ary <- sapply(zeta.ary, FUN = function(zeta) {
#       x1 <- floor(zeta)
#       x2 <- ceiling(zeta)
#       
#       x1 <- min(max(x1,1),n)
#       x2 <- min(max(x2,1),n)
#       
#       y1 <- sorted_x[x1]
#       y2 <- sorted_x[x2]
#       
#       
#       b <- (y2-y1)/(x2-x1)
#       a <- y1-b*x1
#       
#       print(paste("zeta",zeta,"x1",x1,"x2",x2,"y1",y1,"y2",y2,"a",a,"b",b))
#       
#       
#       a+b*zeta
#       
#     })
#     
#     x.zeta.m3 <- x.zeta.ary[1]
#     x.zeta.m1 <- x.zeta.ary[2]
#     x.zeta.1  <- x.zeta.ary[3]
#     x.zeta.3  <- x.zeta.ary[4]
#     
#     est.m <- x.zeta.3 - x.zeta.1
#     est.n <- x.zeta.m1 - x.zeta.m3
#     est.p <- x.zeta.1 - x.zeta.m1
#     
#     print(paste("m3",x.zeta.m3,"m1",x.zeta.m1,"p1",x.zeta.1, "p3", x.zeta.3))
#     
#     
#     print(paste("p",est.p,"m",est.m,"n",est.n))
#     
#     this.iter.su <- list(
#       z             = z
#       ,transform    = "su"
#       ,mn.over.p.sq = est.m*est.n/(est.p^2)
#       ,gamma        = NA
#       ,eta          = NA
#       ,epsilon      = NA
#       ,lambda       = NA
#       
#       ,adtest.AA    = NA
#       ,adtest.p     = NA
#       
#       ,swtest.W     = NA
#       ,swtest.p     = NA
#       
#       ,g3.skewness  = NA
#       ,g3test.z     = NA
#       ,g3test.p     = NA
#       
#       ,g4.kurtosis  = NA
#       ,g4test.z     = NA
#       ,g4test.p     = NA
#       
#       ,pois.test.chi.square = NA
#       ,pois.test.p          = NA
#       
#       ,sw.exp.test.W = NA
#       ,sw.exp.test.p = NA
#     )
#     
#     this.iter.sb <- this.iter.su
#     this.iter.sb$transform <- "sb"
#     this.iter.sl <- this.iter.su
#     this.iter.sl$transform <- "sl"
#     
#     #Johnson SU parameter estimates (Slifker/Shapiro 1980)
#     
#     this.iter.su$eta <- 2*z / acosh(.5*(est.m/est.p + est.n/est.p))
#     
#     this.iter.su$gamma <- this.iter.su$eta*asinh(
#       (est.n / est.p - est.m / est.p) /
#         2*sqrt( (est.m/est.p)*(est.n/est.p) -1  )
#     )
#     
#     this.iter.su$lambda <- (2*est.p*sqrt( (est.m/est.p)*(est.n/est.p) -1  ) / 
#                               ( (est.m/est.p +est.n /est.p -2 ) *sqrt((est.m/est.p +est.n /est.p +2 )))
#     )
#     
#     this.iter.su$epsilon <- ((x.zeta.1 +x.zeta.m1)/2 +
#                                est.p*(est.n/est.p -est.m/est.p) / ( 2*(est.m/est.p +est.n/est.p -2)  )
#                              
#     )
#     
#     #Johnson SB parameter estimates (Slifker/Shapiro 1980)
#     
#     this.iter.sb$eta <- z / ( acosh(.5*sqrt((1+est.p/est.m)*(1+est.p/est.n))) )
#     
#     
#     this.iter.sb$gamma <- this.iter.sb$eta * asinh(
#       ((est.p/est.n - est.p/est.m) * sqrt((1+est.p/est.m)*(1+est.p/est.n) -4)  ) /
#         (2*(est.p/est.m)*(est.p/est.n) -1)
#     )
#     
#     
#     this.iter.sb$lambda <-  (est.p * sqrt( ((1+est.p/est.m)*(1+est.p/est.n) -2)^2  ) /
#                                ((est.p/est.m)*(est.p/est.n) -1))
#     
#     
#     this.iter.sb$epsilon <- (.5*(x.zeta.1 + x.zeta.m1) - this.iter.sb$lambda/2 + 
#                                (est.p*(est.p/est.m - est.p/est.m)) / (2*((est.p/est.m)*(est.p/est.n) -1))
#     )
#     
#     #Johnson SL parameter estimates (Slifker/Shapiro 1980)
#     
#     this.iter.sl$eta <- 2*z/log(est.m/est.p)
#     
#     this.iter.sl$gamma <- this.iter.sl$eta * log((est.m/est.p -1) / (est.p * sqrt(est.m/est.p)))  
#     
#     this.iter.sl$lambda <- 0
#     
#     this.iter.sl$epsilon <- .5*(x.zeta.1 + x.zeta.m1) - (est.p/2)*((est.m/est.p+1)/(est.m/est.p-1))
#     
#     
#     
#     
#     
#     ret <- rbind(ret, this.iter.su, this.iter.sb, this.iter.sl)
#     
#   }
#   
#   
#   
#   options(stringsAsFactors = opt.orig)
#   
#   ret
# }