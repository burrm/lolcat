#' Calculate Unique Subgroup Codes
#' 
#' Calculate subgroup codes. Useful for input into other grouping functions and for performing specific tests (ex. one-way ANOVA on multivariate data).
#'
#' @param fx Formula - defines formula for dependent variable and subgroups ( dv ~ v1 + v2 + ...)
#' @param data Data Frame - data frame with columns corresponding to variables in fx. 
#' @param start.at Scalar - A number to start numbering at. 
#'
#' @return A vector the same length as rows in data containing the results the subgroup code. 


compute.group.cell.codes <- function(
   fx          #Formula defining groups
  ,data = NULL #data frame
  ,start.at = 1
) {
  
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  
  d <- data.frame(dv = data[[response]]
                  ,case = 1:nrow(data))
  
  aggr.fn <- function(d, i, j) {
    #cat(paste("aggr.fn i=",i), fill=T)
    #cat(str(d), fill=T)
    #cat("", fill = T)
    
    #if (!is.data.frame(d)) {
    #  stop("Not a data frame")
    #}
    
    if (is.na(iv.names[i])) {
      if (nrow(d) > 0) {
        d$computed <- j
        j <- j+1
      } else {
        d$computed <- numeric(0)
      }
      
      list(data=d, next.cell.code = j)
      
    } else {
      data[[iv.names[i]]] <- factor(data[[iv.names[i]]], exclude = NULL)
      
      d.next <- split(d, data[[iv.names[i]]][d$case])

      #print(str(d.next))
            
      data.list <- list()
      
      next.code <- j
      for (iter in 1:length(d.next)) {
        tmp <- aggr.fn(d.next[[iter]], i= i+1, j= next.code)
        
        #print(str(tmp))
        
        next.code <- tmp$next.cell.code
        data.list[[iter]] <- tmp$data
      }
      
      #print(str(data.list))
      
      ret <- do.call("rbind", data.list)
      
      ret <- list(data = ret, next.cell.code = next.code)
      
      ret
    }
  }
  
  d <- aggr.fn(d, i = 1, j = start.at)
  
  d <- d$data
  
  #cat("After recursive split", fill=T)
  #cat(str(d), fill=T)
  #cat("", fill = T)
  
  d$computed[order(d$case)]
  
  
  
}