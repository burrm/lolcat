summary.continuous <-function(fx
                              ,data = NULL
                              ,stat.n=T
                              ,stat.total.n=F
                              ,stat.miss=T
                              ,stat.mean=T
                              ,stat.var=T
                              ,stat.ad.test   = 1  # 0 = off, 1 = if n<25, 2 = on
                              ,stat.sw.test   = 1  # 0 = off, 1 = if n<25, 2 = on
                              ,stat.skew.test = 1  # 0 = off, 1 = if n >= 20, 2 = on
                              ,stat.kurt.test = 1  # 0 = off, 1 = if n >= 20, 2 = on
                              ,stat.pois.test = F
                              
                              ,...) {
  summary.impl(fx = fx
               ,data = data
               ,stat.n         = stat.n
               ,stat.total.n   = stat.total.n
               ,stat.miss      = stat.miss
               ,stat.mean      = stat.mean
               ,stat.var       = stat.var
               ,stat.ad.test   = stat.ad.test
               ,stat.sw.test   = stat.sw.test
               ,stat.skew.test = stat.skew.test
               ,stat.kurt.test = stat.kurt.test
               ,stat.pois.test = stat.pois.test
               ,...
  )
}