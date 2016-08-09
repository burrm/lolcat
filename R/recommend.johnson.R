recommend.johnson <- function(x
                              ,z.min = .05
                              ,z.max = 1
                              ,step  = .01
                              
                              ,quantile.type = 8
                              
                              ,target = c("normal")
                              ,metric = c("skewness+kurtosis", "skewness", "kurtosis")
                              
) {

  
  jo.out <- explore.johnson(x
                            ,z.min = z.min
                            ,z.max = z.max
                            ,step = step
                            ,quantile.type = quantile.type
  )
  
  jo.out$sum.g3.g4 <- if (metric[1] == "skewness+kurtosis") {
    abs(jo.out$g3.skewness) + abs(jo.out$g4.kurtosis)  
  } else if (metric[1] == "skewness") {
    abs(jo.out$g3.skewness)
  } else if (metric[1] == "kurtosis") {
    abs(jo.out$g4.kurtosis)
  }
  
  jo.out <- split(jo.out, jo.out$transform)
  
  ret.list <- lapply(jo.out, FUN = function(l) {
    ret <- NULL
    
    if(any(is.finite(l$sum.g3.g4))) {

      ret <- list()
      
      min.g3.g4 <- min(na.omit(l$sum.g3.g4))
      
      row <- which(l$sum.g3.g4 == min.g3.g4)
      selected.row <- as.data.frame(l[row[1],])

      for (i in names(selected.row)) {
        ret[[i]] <- selected.row[[i]][1]
      }
      
            
    }
    
    ret
    
  })

  ret <- do.call(rbind.data.frame, ret.list)
  
  for (i in names(ret)) {
    if (all(is.na(ret[[i]]))) {
      ret[[i]] <- NULL
    }
  }
  
  for (i in c("g3test.z", "z", "g4test.z", "sum.g3.g4", "mn.over.p.sq")) {
    ret[[i]] <- NULL
  }
  
  ret
}