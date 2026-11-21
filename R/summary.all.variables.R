#' @rdname summary.impl
summary.all.variables <- function(data, summary.default = summary.continuous , ...) {
  var.names <- names(data)
  
  fx <- eval(parse(text=paste(var.names[1],"~ 1")))
  ret <- summary.default(fx, data=data, ...)
  
  #print(ret)
  
  if (length(var.names) > 1) {
    for (i in 2:length(var.names)) {
      fx <- eval(parse(text=paste(var.names[i],"~ 1")))
      ret2 <- summary.default(fx, data=data, ...)
      #print(ret2)
      ret <- rbind(ret, ret2)
    }
  }
  
  ret
}