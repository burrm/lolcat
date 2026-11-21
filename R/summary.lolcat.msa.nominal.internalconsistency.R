summary.lolcat.msa.nominal.internalconsistency <- function(object, ...) {
  ret <- data.frame()
  
  conf.level <- object$conf.level
  
  sig.p.value <- 1-conf.level
  comp <- object$comparisons 
  
  ret <- data.frame(Description = names(comp), stringsAsFactors = F)
  
  n.agree <- unlist(lapply(comp, FUN = function(o) { 
      utility.extract.named.value(o$agreement$estimate, "n.agree")
  }))
  
  n.disagree <- unlist(lapply(comp, FUN = function(o) { 
    utility.extract.named.value(o$agreement$estimate, "n.disagree")
  }))
  
  n <- n.agree + n.disagree
  
  kappa.max <- unlist(lapply(comp, FUN = function(o) { 
    utility.extract.named.value(o$agreement$estimate, "kappa.max")
  }))

  kappa <- unlist(lapply(comp, FUN = function(o) { 
    utility.extract.named.value(o$agreement$estimate, "kappa")
  }))
  
  p.value <-  unlist(lapply(comp, FUN = function(o) { 
    o$agreement$p.value
  }))

  p.chance <- unlist(lapply(comp, FUN = function(o) { 
    utility.extract.named.value(o$agreement$estimate, "p_c")
  }))
  
    
  ci.low <- unlist(lapply(comp, FUN = function(o) { 
    o$agreement$conf.int[1]
  }))
  
  ci.high <- unlist(lapply(comp, FUN = function(o) { 
    o$agreement$conf.int[2]
  }))
  
  symmetry.chisq <- unlist(lapply(comp, FUN = function(o) { 
    o$symmetry$statistic
  }))
  
  symmetry.p.value <- unlist(lapply(comp, FUN = function(o) { 
    o$symmetry$p.value
  }))
  
  ret$kappa      <- rmnames(kappa)
  ret$kappa.p.value    <- rmnames(p.value)
  ret$kappa.ci.low     <- rmnames(ci.low)
  ret$kappa.ci.high    <- rmnames(ci.high)
  ret$kappa.max  <- rmnames(kappa.max)
  ret$n          <- rmnames(n)
  ret$n.agree    <- rmnames(n.agree)
  ret$n.disagree <- rmnames(n.disagree)
  ret$p.agree    <- ret$n.agree / ret$n
  ret$p.disagree <- ret$n.disagree / ret$n
  ret$p.chance   <- rmnames(p.chance)
  ret$symmetry.chisq <- rmnames(symmetry.chisq)
  ret$symmetry.p.value <- rmnames(symmetry.p.value)
  
  ret
}

