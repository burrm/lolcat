#Adapted from http://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
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
