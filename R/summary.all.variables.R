summary.all.variables <- function(data, summary.default = summary.continuous , ...) {
  var.names <- names(data)
  
  fx <- eval(parse(text=paste(var.names[1],"~ 1")))
  ret <- summary.default(fx, data=data, ...)
  
  if (length(var.names) > 1) {
    for (i in 2:length(var.names)) {
      fx <- eval(parse(text=paste(var.names[i],"~ 1")))
      ret <- rbind(ret, summary.default(fx, data=data, ...))
    }
  }
  
  ret
}