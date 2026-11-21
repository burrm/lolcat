#' Calculate Subgroup Variable
#' 
#' Calculate subgroup variable using an arbitrary function.
#'
#' @param fx Formula - defines formula for dependent variable and subgroups ( dv ~ v1 + v2 + ...)
#' @param data Data Frame - data frame with columns corresponding to variables in fx. 
#' @param FUN Function - A function(x) where x is subgroup values that returns a vector that is the same length as x. 
#'
#' @return A vector the same length as rows in data containing the results of the subgroup calculation. 

compute.group.variable <- function(
   fx          #Formula defining groups
  ,data = NULL #data frame
  ,FUN = function(x) {
            x #Identity function
         }
) {
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  
  d <- data.frame(dv = data[[response]]
                  ,case = 1:nrow(data))
  
  aggr.fn <- function(d, i) {
    #cat(paste("aggr.fn i=",i), fill=T)
    #cat(str(d), fill=T)
    #cat("", fill = T)

    #if (!is.data.frame(d)) {
    #  stop("Not a data frame")
    #}
    
    if (is.na(iv.names[i])) {
      d$computed <- FUN(d$dv)
      
      d
      
    } else {
      data[[iv.names[i]]] <- factor(data[[iv.names[i]]], exclude = NULL)
      
      d.next <- split(d, data[[iv.names[i]]][d$case])
        
      ret <- lapply(d.next, function(y) { aggr.fn(y, i+1) })
        
      ret <- do.call("rbind", ret)
                
      ret
    }
  }
  
  d <- aggr.fn(d, 1)

  #cat("After recursive split", fill=T)
  #cat(str(d), fill=T)
  #cat("", fill = T)

  d$computed[order(d$case)]
  
}

