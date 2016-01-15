kurtosis.test <-
function(x
         ,conf.level = .95
         #,type = 2
         )
{
    n = length(x)
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a4 = mean((x-meanX)**4)/s**4
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U4 = (a4-3+6/(n+1))/SD4
    B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
    jm = sqrt(2/(9*A))
    pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
    Z4 = (1-2/(9*A)-pos)/jm
    pZ4 = 2*(1-pnorm(abs(Z4),0,1))
    names(Z4) = "Z4"
    
    ku<-kurtosis(x)
    
    cv<-qnorm(conf.level+(1-conf.level)/2)
    se.sk   <- sqrt(6*n*(n-1)/((n-2)*(n+1)*(n+3)))
    se.est  <- 2*se.sk*sqrt((n^2-1)/((n-3)*(n+5)))
    ciupper <- ku+cv*se.est
    cilower <- ku-cv*se.est
    
    retval<-list(data.name   = deparse(substitute(x)),
                 statistic   = ku, 
                 estimate    = c(kurtosis=ku, z=rmnames(Z4), se.est=se.est),
                 parameter   = 0 ,
                 p.value     = pZ4,
                 null.value  = 0,
                 alternative = "two.sided",
                 method      = "D'Agostino Kurtosis Normality Test",
                 conf.int    = c(cilower,ciupper)
    )
    
    #names(retval$estimate) <- c("sample kurtosis","z statistic")
    names(retval$statistic) <- "kurtosis"
    names(retval$null.value) <- "kurtosis"
    names(retval$parameter) <- "null hypothesis kurtosis"
    attr(retval$conf.int, "conf.level")  <- conf.level
    
    class(retval)<-"htest"
    retval
    
}