summary.impl <- function(fx 
                         ,data
                         ,stat.n           = F
                         ,stat.total.n     = F
                         ,stat.miss        = F
                         ,stat.sum         = F
                         ,stat.mean        = F
                         ,stat.var         = F
                         ,stat.sd          = F
                         ,stat.mean.ADA    = F
                         ,stat.mean.ADM    = F
                         ,stat.mean.ADMn1  = F
                         
                         #Ordinal things
                         ,stat.quantiles   = NULL # vector of values between 0 and 1
                         ,stat.five.number = F
                         ,stat.min         = F
                         ,stat.q1          = F
                         ,stat.median      = F
                         ,stat.q3          = F
                         ,stat.max         = F
                         ,stat.range       = F
                         ,stat.iqr         = F
                         ,stat.psd         = F
                         ,stat.sir         = F
                         ,stat.coefvar     = F
                         
                         #Nominal things
                         ,stat.distinct        = F #Number of distinct values in DV, excluding NA
                         ,stat.distinct.withna = F #Number of distinct values in DV, including NA
                        
                         #Misc things
                         ,stat.true.mode   = F
                          
                         #Shape Testing
                         ,stat.shape.rejection.conf.level = NA #0 < p < 1
                         ,stat.shape.text.rej = "Reject"
                         ,stat.shape.text.ftr = ""
                         ,stat.ad.test   = 0  # 0 = off, 1 = if n<25, 2 = on
                         ,stat.sw.test   = 0  # 0 = off, 1 = if n<25, 2 = on
                         ,stat.skew.test = 0  # 0 = off, 1 = if n >= 20, 2 = on
                         ,stat.kurt.test = 0  # 0 = off, 1 = if n >= 20, 2 = on
                         ,stat.pois.dist.test = F
                         ,stat.sw.exp.test = F

                         #Misc...
                         ,stat.sd.report = NULL # vector of values to report sd*val
                         
                         #Specification Limit Related Stuff
                         ,stat.lsl               = NA
                         ,stat.target            = NA
                         ,stat.usl               = NA
                         ,stat.nonconform.nbelow = F
                         ,stat.nonconform.nabove = F
                         ,stat.nonconform.nout   = F
                         ,stat.nonconform.pbelow = F
                         ,stat.nonconform.pabove = F
                         ,stat.nonconform.pout   = F
                         
                         #Formatting Stuff
                         ,format.generate.cellcodes = F
                         
                         
                         ,...
) {
  argss <- c(as.list(environment()), list(...))
  
  
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  iv.names<-attributes(terms(fx))$term.labels
  
  d.samplesizes<-as.integer(aggregate(fx,data = data, function(x) {length(na.omit(x))})[,(length(iv.names)+1)])
  

  #Process statistics selections
  d.summary<-aggregate(fx, data = data, na.action = na.pass, FUN = function(x) {
    #agg<-numeric(0)
    clean_x <- na.omit(x)

    argss$x <- x
    argss$clean_x <- clean_x
    argss$agg <- numeric(0)

    if (is.factor(clean_x) | ! is.numeric(clean_x)) {
      agg <- do.call(.summary.impl.factor, list(argss))
    } else {
      agg <- do.call(.summary.impl.numeric, list(argss))
    }
    
    agg
  })
  
  #print(str(d.summary))
  
  #TODO - better way to post process :(
  #Post-process to correct format 
  #   - multi-return aggregate puts a matrix into data frame :/
  if (length(iv.names) > 0) {
    d.final<-as.data.frame(d.summary[,1:length(iv.names)])
    names(d.final)[1:length(iv.names)]<-iv.names
    d.final<-cbind(d.final,d.summary[[(length(iv.names)+1)]])
    names(d.final)[(length(iv.names)+1):ncol(d.final)]<-dimnames(d.summary[[(length(iv.names)+1)]])[[2]]
  } else {
    d.final <- as.data.frame(d.summary[[response]])
  }
  
  #d.final<-as.data.frame(d.final[[1]])
  
  #Final post-processing - delete/reformat a few columns
  # - shape testing - if eq 1 and all gp size, then delete
  
  delete_condition <- all(d.samplesizes < 20)
  if (stat.skew.test == 1 & delete_condition) {
    d.final[["g3.skewness"]] <- NULL
    d.final[["g3test.p"]] <- NULL
    d.final[["g3test.d"]] <- NULL
  }
  
  if (stat.kurt.test == 1 & delete_condition) {
    d.final[["g4.kurtosis"]] <- NULL
    d.final[["g4test.p"]] <- NULL
    d.final[["g4test.d"]] <- NULL
  }
  
  delete_condition <- all(d.samplesizes >= 25)
  if (stat.ad.test == 1 & delete_condition) {
    d.final[["adtest.AA"]] <- NULL
    d.final[["adtest.p"]] <- NULL
    d.final[["adtest.d"]] <- NULL
  }
  
  if (stat.sw.test == 1 & delete_condition) {
    d.final[["swtest.W"]] <- NULL
    d.final[["swtest.p"]] <- NULL
    d.final[["swtest.d"]] <- NULL
  }

  if (is.na(stat.shape.rejection.conf.level)) {
    d.final[["g3test.d"]] <- NULL
    d.final[["g4test.d"]] <- NULL
    d.final[["adtest.d"]] <- NULL
    d.final[["swtest.d"]] <- NULL
    d.final[["pois.test.d"]] <- NULL
  }

  #Final formatting of shape decision
  for (i in c("g3test.d", "g4test.d", "adtest.d", "swtest.d", "pois.test.d", "sw.exp.test.d")) {
    if (any(names(d.final) == i)) {
      d.final[[i]] <- ifelse(d.final[[i]] == 1, stat.shape.text.rej, stat.shape.text.ftr)
      d.final[[i]] <- factor(d.final[[i]], levels=c(stat.shape.text.ftr, stat.shape.text.rej))
    }
  }
    
  if (length(iv.names) > 0) {
    if (length(iv.names) > 1) {
      # Sort by independent variables
      d.final<-d.final[do.call(order,d.final[,1:length(iv.names)]),]
      rownames(d.final)<-1:nrow(d.final)
    } else {
      d.final<-d.final[order(d.final[,1]),]
      rownames(d.final)<-1:nrow(d.final)
    }
    
    # Perform formatting stuff...
    if (format.generate.cellcodes) {
      d.final<-cbind(d.final[,1:length(iv.names)],cell.code=rep(NA,nrow(d.final)),d.final[,(length(iv.names)+1):ncol(d.final)])
      names(d.final)[1:length(iv.names)]<-iv.names
      for (i in 1:nrow(d.final)) {
        tn<-paste("Cell ",i," - ",sep="")
        #print(paste("220:'",tn,"'",sep=""))
        for (j in 1:length(iv.names)) {
          if (j > 1) {
            tn <- paste(tn, ", ",sep="") 
            #print(paste("224:'",tn,"'",sep=""))
          }
          tn <- paste(tn
                      ,iv.names[j]
                      ," "
                      ,d.final[i,j]
                      ,sep="")
          #print(paste("230:'",tn,"'",sep=""))
        }
        d.final[["cell.code"]][i]<-tn
      }
      
      d.final[["cell.code"]]<-factor(d.final[["cell.code"]], levels=d.final[["cell.code"]])
    }
  } else {
    #d.final <- as.data.frame(d.final[1,2])
    
    for (i in ncol(d.final):1) {
      d.final[[i+1]]<-d.final[[i]]
      names(d.final)[i+1]<-names(d.final)[i]
    }
    names(d.final)[1]<-"dv.name"
    d.final[1,1]<-response
  }
  
  #Return summary data frame
  d.final
}

















.summary.impl.numeric <- function(...) {
  argss <- as.list(...)
  
  for ( i in names(argss)) {
    assign(i, argss[[i]])
  }
  
  saved.n    <- length(clean_x)
  saved.mean <- mean(clean_x)
  saved.var  <- var(clean_x)
  saved.sd   <- sqrt(saved.var)
  saved.iqr  <- IQR(clean_x)
  
  #print(paste("length x",length(x), " anyna", anyNA(x)))
  
  #Basic Stuff
  if (stat.n)       { agg<-c(agg,n       = saved.n) }
  if (stat.total.n) { agg<-c(agg,total.n = length(x)) }
  if (stat.miss)    { agg<-c(agg,missing = length(x)-saved.n) }
  if (stat.sum)     { agg<-c(agg,sum     = sum(clean_x)) }
  if (stat.mean)    { agg<-c(agg,mean    = saved.mean) }
  if (stat.var)     { agg<-c(agg,var     = saved.var) }
  if (stat.sd)      { agg<-c(agg,sd      = saved.sd) } 
  if (stat.mean.ADA) { agg <- c(agg,mean.ADA = mean(dispersion.ADA(clean_x))) }
  if (stat.mean.ADM) { agg <- c(agg,mean.ADM = mean(dispersion.ADM(clean_x))) }
  if (stat.mean.ADMn1) { agg <- c(agg,mean.ADMn1 = mean(dispersion.ADMn1(clean_x))) }
  
  #Ordinal Stuff
  if (stat.min | stat.five.number)  { agg<-c(agg,min=min(clean_x)) }
  if (stat.q1 | stat.five.number) { 
    tv <- quantile(clean_x, probs = .25) 
    names(tv)<-NULL
    agg<-c(agg,quartile.1 = tv)
  }
  if (stat.median | stat.five.number)  { agg<-c(agg,median  = median(clean_x)) }
  if (stat.q3 | stat.five.number) { 
    tv <- quantile(clean_x, probs = .75) 
    names(tv)<-NULL
    agg<-c(agg,quartile.3      = tv)
  }
  if (stat.max | stat.five.number)  { agg<-c(agg,max=max(clean_x)) }
  if (length(stat.quantiles) > 0) {
    quantiles.t <- quantile(clean_x,probs=stat.quantiles)
    names(quantiles.t)<-sapply(stat.quantiles, FUN = function(p) { paste("percentile.", p*100, sep="")  })
    for (i in 1:length(quantiles.t)) {
      tv <- quantiles.t[i]
      names(tv)<-names(quantiles.t)[i]
      agg<-c(agg,tv)
    }
  }
  if (stat.range) { agg<-c(agg,range=max(clean_x)-min(clean_x))}
  if (stat.iqr)  { agg<-c(agg,iqr=IQR(clean_x)) }
  if (stat.psd) { agg<-c(agg,psd=saved.iqr/1.35)  }
  if (stat.sir) { agg<-c(agg,sir=saved.iqr/2) } 
  if (stat.coefvar) { agg<-c(agg,coefvar=saved.sd/saved.mean) }   
  
  #Nominal stuff
  if (stat.distinct) { agg<-c(agg,distinct=length(table(clean_x))) }
  if (stat.distinct.withna) { agg<-c(agg,distinct.withna=length(table(x, useNA="ifany"))) }
  
  #Misc stuff
  if (stat.true.mode) { agg <-c(agg,true.mode = true.mode(clean_x))}
  
  
  #Shape testing
  
  if (stat.ad.test > 0 & saved.n > 1) {
    t.res <- anderson.darling.test(clean_x)
    agg<-c(agg,adtest = t.res$statistic)
    agg<-c(agg,adtest.p  = t.res$p.value)
    agg<-c(agg,adtest.d  = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.ad.test > 0) {
    agg<-c(agg,adtest.AA = NA)
    agg<-c(agg,adtest.p = NA)
    agg<-c(agg,adtest.d = NA)
  }
  
  if (stat.sw.test > 0 & saved.n > 2 & saved.n < 5000) {
    t.res <- shapiro.test(clean_x)
    agg<-c(agg,swtest = t.res$statistic)
    agg<-c(agg,swtest.p  = t.res$p.value)
    agg<-c(agg,swtest.d  = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.sw.test > 0) {
    agg<-c(agg,swtest.W = NA)
    agg<-c(agg,swtest.p = NA)
    agg<-c(agg,swtest.d = NA)
  }
  
  if (stat.skew.test > 0 & saved.n > 3) {
    t.res <- skewness.test(clean_x)
    agg<-c(agg,g3        = t.res$statistic)
    agg<-c(agg,g3test.p  = t.res$p.value)
    agg<-c(agg,g3test.d  = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.skew.test > 0) {
    agg<-c(agg,g3.skewness = NA)
    agg<-c(agg,g3test.p  = NA)
    agg<-c(agg,g3test.d  = NA)
  }
  
  if (stat.kurt.test > 0 & saved.n > 4) {
    t.res <- kurtosis.test(clean_x)
    agg<-c(agg,g4        = t.res$statistic)
    agg<-c(agg,g4test.p  = t.res$p.value)
    agg<-c(agg,g4test.d  = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.kurt.test > 0) {
    agg<-c(agg,g4.kurtosis = NA)
    agg<-c(agg,g4test.p  = NA)
    agg<-c(agg,g4test.d  = NA)
  }
  
  if (stat.pois.dist.test & saved.n > 2) {
    t.res <- poisson.dist.test(clean_x)
    agg<-c(agg,pois.test        = t.res$statistic)
    agg<-c(agg,pois.test.p      = t.res$p.value)
    agg<-c(agg,pois.test.d      = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.pois.dist.test) {
    agg<-c(agg,pois.test.chi.square = NA)
    agg<-c(agg,pois.test.p  = NA)
    agg<-c(agg,pois.test.d  = NA)
  }
  
  if (stat.sw.exp.test & saved.n > 2) {
    t.res <- shapiro.wilk.exponentiality.test(clean_x)
    agg<-c(agg,sw.exp.test      = t.res$statistic)
    agg<-c(agg,sw.exp.test.p      = t.res$p.value)
    agg<-c(agg,sw.exp.test.d      = t.res$p.value < 1-stat.shape.rejection.conf.level)
  } else if (stat.sw.exp.test) {
    agg<-c(agg,sw.exp.test.W = NA)
    agg<-c(agg,sw.exp.test.p  = NA)
    agg<-c(agg,sw.exp.test.d  = NA)
  }
  
  if (length(stat.sd.report) > 0) {
    for ( i in stat.sd.report) {
      tv<-i*saved.sd
      names(tv)<-if (i < 0) {
        paste("sd.x.m",abs(i),sep="")
      } else {
        paste("sd.x.",i,sep="")
      }
      agg<-c(agg,tv)
    }
  }
  
  saved.sl.above <- 0
  saved.sl.below <- 0
  
  if (!is.na(stat.lsl)) {
    agg<-c(agg,spec.lsl  = stat.lsl)
    saved.sl.below <- sum(as.integer(clean_x < stat.lsl))
  }        
  
  if (!is.na(stat.target)) {
    agg<-c(agg,spec.tgt  = stat.target)
  }        
  
  if (!is.na(stat.usl)) {
    agg<-c(agg,spec.usl  = stat.usl)
    saved.sl.above <- sum(as.integer(clean_x > stat.usl))
  }
  
  if (stat.nonconform.nbelow) { agg<-c(agg, spec.nbelow = saved.sl.below) }
  if (stat.nonconform.nabove) { agg<-c(agg, spec.nabove = saved.sl.above) }
  if (stat.nonconform.nout)   { agg<-c(agg, spec.nout = saved.sl.below + saved.sl.above) }
  if (stat.nonconform.pbelow) { agg<-c(agg, spec.pbelow = saved.sl.below / saved.n) }
  if (stat.nonconform.pabove) { agg<-c(agg, spec.pabove = saved.sl.above / saved.n) }
  if (stat.nonconform.pout)   { agg<-c(agg, spec.pout = (saved.sl.below + saved.sl.above) / saved.n) }

  agg  
}



















.summary.impl.factor <- function(...) {
  argss <- as.list(...)
  
  for ( i in names(argss)) {
    assign(i, argss[[i]])
  }
  
  saved.n    <- length(clean_x)
  saved.mean <- NA
  saved.var  <- NA
  saved.sd   <- NA
  saved.iqr  <- NA
  
  #print(paste("length x",length(x), " anyna", anyNA(x)))
  
  #Basic Stuff
  if (stat.n)       { agg<-c(agg,n       = saved.n) }
  if (stat.total.n) { agg<-c(agg,total.n = length(x)) }
  if (stat.miss)    { agg<-c(agg,missing = length(x)-saved.n) }
  if (stat.sum)     { agg<-c(agg,sum     = NA) }
  if (stat.mean)    { agg<-c(agg,mean    = saved.mean) }
  if (stat.var)     { agg<-c(agg,var     = saved.var) }
  if (stat.sd)      { agg<-c(agg,sd      = saved.sd) } 
  if (stat.mean.ADA) { agg <- c(agg,mean.ADA = NA) }
  if (stat.mean.ADM) { agg <- c(agg,mean.ADM = NA) }
  if (stat.mean.ADMn1) { agg <- c(agg,mean.ADMn1 = NA) }
  
  #Ordinal Stuff
  if (stat.min | stat.five.number)  { agg<-c(agg,min=NA) }
  if (stat.q1 | stat.five.number) { 
    #tv <- quantile(clean_x, probs = .25) 
    #names(tv)<-NULL
    agg<-c(agg,quartile.1 = NA)
  }
  if (stat.median | stat.five.number)  { agg<-c(agg,median  = NA) }
  if (stat.q3 | stat.five.number) { 
    #tv <- quantile(clean_x, probs = .75) 
    #names(tv)<-NULL
    agg<-c(agg,quartile.3      = NA)
  }
  if (stat.max | stat.five.number)  { agg<-c(agg,max=NA) }
  if (length(stat.quantiles) > 0) {
    quantiles.t <- rep(NA, length(stat.quantiles))
    names(quantiles.t)<-sapply(stat.quantiles, FUN = function(p) { paste("percentile.", p*100, sep="")  })
    for (i in 1:length(quantiles.t)) {
      tv <- quantiles.t[i]
      names(tv)<-names(quantiles.t)[i]
      agg<-c(agg,tv)
    }
  }
  if (stat.range) { agg<-c(agg,range=NA)}
  if (stat.iqr)  { agg<-c(agg,iqr=NA) }
  if (stat.psd) { agg<-c(agg,psd=NA)  }
  if (stat.sir) { agg<-c(agg,sir=NA) } 
  if (stat.coefvar) { agg<-c(agg,coefvar=NA) }   
  
  #Nominal stuff
  if (stat.distinct) { agg<-c(agg,distinct=length(table(clean_x))) }
  if (stat.distinct.withna) { agg<-c(agg,distinct.withna=length(table(x, useNA="ifany"))) }
  
  #Misc stuff
  if (stat.true.mode) { agg <-c(agg,true.mode = NA)}
  
  
  #Shape testing
  
  if (stat.ad.test > 0 & saved.n > 1) {
    #t.res <- anderson.darling.test(clean_x)
    agg<-c(agg,adtest.AA = NA)
    agg<-c(agg,adtest.p  = NA)
    agg<-c(agg,adtest.d  = NA)
  } else if (stat.ad.test > 0) {
    agg<-c(agg,adtest.AA = NA)
    agg<-c(agg,adtest.p = NA)
    agg<-c(agg,adtest.d = NA)
  }
  
  if (stat.sw.test > 0 & saved.n > 2 & saved.n < 5000) {
    #t.res <- shapiro.test(clean_x)
    agg<-c(agg,swtest.W = NA)
    agg<-c(agg,swtest.p  = NA)
    agg<-c(agg,swtest.d  = NA)
  } else if (stat.sw.test > 0) {
    agg<-c(agg,swtest.W = NA)
    agg<-c(agg,swtest.p = NA)
    agg<-c(agg,swtest.d = NA)
  }
  
  if (stat.skew.test > 0 & saved.n > 3) {
    #t.res <- skewness.test(clean_x)
    agg<-c(agg,g3.skewness = NA)
    agg<-c(agg,g3test.p  = NA)
    agg<-c(agg,g3test.d  = NA)
  } else if (stat.skew.test > 0) {
    agg<-c(agg,g3.skewness = NA)
    agg<-c(agg,g3test.p  = NA)
    agg<-c(agg,g3test.d  = NA)
  }
  
  if (stat.kurt.test > 0 & saved.n > 4) {
    #t.res <- kurtosis.test(clean_x)
    agg<-c(agg,g4.kurtosis = NA)
    agg<-c(agg,g4test.p  = NA)
    agg<-c(agg,g4test.d  = NA)
  } else if (stat.kurt.test > 0) {
    agg<-c(agg,g4.kurtosis = NA)
    agg<-c(agg,g4test.p  = NA)
    agg<-c(agg,g4test.d  = NA)
  }
  
  if (stat.pois.dist.test & saved.n > 2) {
    #t.res <- poisson.dist.test(clean_x)
    agg<-c(agg,pois.test.chi.square = NA)
    agg<-c(agg,pois.test.p      = NA)
    agg<-c(agg,pois.test.d      = NA)
  } else if (stat.pois.dist.test) {
    agg<-c(agg,pois.test.chi.square = NA)
    agg<-c(agg,pois.test.p  = NA)
    agg<-c(agg,pois.test.d  = NA)
  }

  if (stat.sw.exp.test & saved.n > 2) {
    #t.res <- poisson.dist.test(clean_x)
    agg<-c(agg,sw.exp.test.W = NA)
    agg<-c(agg,sw.exp.test.p = NA)
    agg<-c(agg,sw.exp.test.d = NA)
  } else if (stat.sw.exp.test) {
    agg<-c(agg,sw.exp.test.W = NA)
    agg<-c(agg,sw.exp.test.p  = NA)
    agg<-c(agg,sw.exp.test.d  = NA)
  }
    
  if (length(stat.sd.report) > 0) {
    for ( i in stat.sd.report) {
      tv<-i*saved.sd
      names(tv)<-if (i < 0) {
        paste("sd.x.m",abs(i),sep="")
      } else {
        paste("sd.x.",i,sep="")
      }
      agg<-c(agg,tv)
    }
  }
  
  saved.sl.above <- NA
  saved.sl.below <- NA
  
  if (!is.na(stat.lsl)) {
    agg<-c(agg,spec.lsl  = NA)
    saved.sl.below <- NA
  }        
  
  if (!is.na(stat.target)) {
    agg<-c(agg,spec.tgt  = NA)
  }        
  
  if (!is.na(stat.usl)) {
    agg<-c(agg,spec.usl  = NA)
    saved.sl.above <- NA
  }
  
  if (stat.nonconform.nbelow) { agg<-c(agg, spec.nbelow = NA) }
  if (stat.nonconform.nabove) { agg<-c(agg, spec.nabove = NA) }
  if (stat.nonconform.nout)   { agg<-c(agg, spec.nout = NA) }
  if (stat.nonconform.pbelow) { agg<-c(agg, spec.pbelow = NA) }
  if (stat.nonconform.pabove) { agg<-c(agg, spec.pabove = NA) }
  if (stat.nonconform.pout)   { agg<-c(agg, spec.pout = NA) }

  agg  
}