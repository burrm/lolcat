process.binary.splits <- function(s) {
  m <- 2^length(s) -1
  
  ll <- lapply(1:(2^length(s) -1), FUN = function(i) {
    m_complement = bitwXor(i, m)
    
    if (i > m_complement) {
      NULL 
    } else {
      bs <- intToBits(i)
      
      idx.s1 <- which(bs == as.raw(1))
      
      if (length(idx.s1) < length(s) & length(idx.s1) > 0) {
        
        s1 <- s[idx.s1]
        s2 <- s[-idx.s1]
        
        ret <- list(set.1 = s1, set.2 = s2)
        
        ret
      }
      
    }
  })
  
  ll <- ll[!sapply(ll, is.null)]
  
  ll
}