chi.square.2d.expected.frequencies <- function(observed.frequencies) {
  ret <- observed.frequencies

  nc <- ncol(observed.frequencies)
  nr <- nrow(observed.frequencies)
    
  observed.frequencies <- xt.sums(observed.frequencies)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      ret[i,j] <-  observed.frequencies[i,(nc+1)]*observed.frequencies[(nr+1),j]/observed.frequencies[(nr+1),(nc+1)]
    }
  }
  
  ret
}