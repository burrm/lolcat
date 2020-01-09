natural.tolerance.poisson.simple <- function(
    lambda = 5
    ,total.area = 1-.9973
    ,upper.tail = total.area / 2
    ,lower.tail = total.area / 2
    ,details = T
) {
    
    ret <- data.frame(natural.tolerance = numeric(0)
               ,lower.limit = numeric(0)
               ,upper.limit = numeric(0)
               ,lower.area = numeric(0)
               ,upper.area = numeric(0) 
               )

    for (i in 1:length(lambda)) {
        p.table <- table.dist.poisson(lambda = lambda[i])

        idx.lower   <- ifelse(any(p.table$eq.and.below <= lower.tail),max(which(p.table$eq.and.below <= lower.tail)),NA)
        lower.limit <- ifelse(!is.na(idx.lower), p.table$x[idx.lower],NA)
        lower.area  <- ifelse(!is.na(idx.lower), p.table$eq.and.below[idx.lower], NA)

        idx.upper   <- ifelse(any(p.table$eq.and.above <= lower.tail), min(which(p.table$eq.and.above <= lower.tail)), NA)
        upper.limit <- ifelse(!is.na(idx.upper),p.table$x[idx.upper],NA)
        upper.area  <- ifelse(!is.na(idx.upper),p.table$eq.and.above[idx.upper],NA)

        nt <- upper.limit - lower.limit

        ret <- rbind(ret,
            data.frame(
                natural.tolerance = nt
               ,lower.limit = lower.limit
               ,upper.limit = upper.limit
               ,lower.area = lower.area
               ,upper.area = upper.area 
               )
        )

    }

  if (!details) {
    ret$natural.tolerance
  } else {
    ret
  }

    
}