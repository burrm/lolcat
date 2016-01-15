rmnames<-function(x) {
  ret <- x
  names(ret)<-NULL
  ret
}

true.mode<-function(x) {
  3*median(x)-2*mean(x)
}
