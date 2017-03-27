format.wrap.labels<-function(x, width = 15) {
  if (is.list(x))
  {
    lapply(x, wrap.labels, width)
  } else {
    sapply(x
           ,function(y) paste(strwrap(y, width)
           ,collapse = "\n"), 
           USE.NAMES = FALSE
           )
  }
  
}
