transform.independent.format.to.dependent.format <- function(
  fx          #Formula defining groups
  ,data = NULL #data frame
  ,start.at = 1
) {
  
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  #iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  cell <- compute.group.cell.codes(fx = fx, data = data)
  
  dv.split <- split(data[[response]], cell)
  
  max_len <- max(unlist(lapply(dv.split, length)))
  
  ret <- data.frame(cell.1 = numeric(max_len))
  
  for ( i in 1:length(dv.split)) {
    ret[[paste("cell.",i,sep="")]] <- c(dv.split[[i]], rep(NA, (max_len - length(dv.split[[i]]))))
  }
  
  ret
}