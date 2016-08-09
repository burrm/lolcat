poisson.test.onesample.simple<-function(
   sample.count
  ,sample.size
  ,null.hypothesis.lambda = 1
  ,alternative = c("two.sided","less","greater")
  ,conf.level = 0.95
) {
  
  #
  # Note: commented section is approximate test (numerics of poisson calculation)
  #
  
  # alpha.ci<-(1-conf.level)/2
  # 
  # ciupper <- qchisq(1-alpha.ci, 2*sample.lambda+2)/2
  # cilower <- qchisq(alpha.ci, 2*sample.lambda)/2
  # 
  # p.value <- if (alternative[1] == "two.sided") {
  #   tmp<-if (sample.lambda <= null.hypothesis.lambda) {
  #     ppois(sample.lambda, lambda = null.hypothesis.lambda, lower.tail = T)
  #   } else {
  #     ppois(sample.lambda, lambda = null.hypothesis.lambda, lower.tail = F)
  #   }
  # 
  #   tmp*2
  # } else if (alternative[1] == "greater") {
  #   ppois(sample.lambda, lambda = null.hypothesis.lambda , lower.tail = FALSE)
  # } else if (alternative[1] == "less") {
  #   ppois(sample.lambda, lambda = null.hypothesis.lambda ,lower.tail = TRUE)
  # } else {
  #   NA
  # }
  # 
  # retval<-list(data.name   = deparse(substitute(sample.lambda)),
  #              statistic   = sample.lambda, 
  #              estimate    = c(sample.lambda = sample.lambda),
  #              parameter   = null.hypothesis.lambda,
  #              p.value     = p.value,
  #              null.value  = null.hypothesis.lambda,
  #              alternative = alternative[1],
  #              method      = "One-Sample Poisson Test for Location",
  #              conf.int    = c(cilower,ciupper)
  # )
  # 
  # #names(retval$estimate) <- c("sample mean")
  # names(retval$statistic) <- "lambda"
  # names(retval$null.value) <- "lambda"
  # names(retval$parameter) <- "null hypothesis lambda"
  # attr(retval$conf.int, "conf.level")  <- conf.level
  # 
  # class(retval)<-"htest"
  # retval
  
  poisson.test(x=sample.count, T= sample.size, r=null.hypothesis.lambda, alternative = alternative[1], conf.level = conf.level)
  
  
  
  
  #TODO: Extend built in implementation
  # function (x, T = 1, r = 1, alternative = c("two.sided", "less", 
  #                                            "greater"), conf.level = 0.95) 
  # {
  #   DNAME <- deparse(substitute(x))
  #   DNAME <- paste(DNAME, "time base:", deparse(substitute(T)))
  #   if ((l <- length(x)) != length(T)) 
  #     if (length(T) == 1L) 
  #       T <- rep(T, l)
  #   else stop("'x' and 'T' have incompatible length")
  #   xr <- round(x)
  #   if (any(!is.finite(x) | (x < 0)) || max(abs(x - xr)) > 1e-07) 
  #     stop("'x' must be finite, nonnegative, and integer")
  #   x <- xr
  #   if (any(is.na(T) | (T < 0))) 
  #     stop("'T' must be nonnegative")
  #   if ((k <- length(x)) < 1L) 
  #     stop("not enough data")
  #   if (k > 2L) 
  #     stop("the case k > 2 is unimplemented")
  #   if (!missing(r) && (length(r) > 1 || is.na(r) || r < 0)) 
  #     stop("'r' must be a single positive number")
  #   alternative <- match.arg(alternative)
  #   if (k == 2) {
  #     RVAL <- binom.test(x, sum(x), r * T[1L]/(r * T[1L] + 
  #                                                T[2L]), alternative = alternative, conf.level = conf.level)
  #     RVAL$data.name <- DNAME
  #     RVAL$statistic <- c(count1 = x[1L])
  #     RVAL$parameter <- c(`expected count1` = sum(x) * r * 
  #                           T[1L]/sum(T * c(1, r)))
  #     RVAL$estimate <- c(`rate ratio` = (x[1L]/T[1L])/(x[2L]/T[2L]))
  #     pp <- RVAL$conf.int
  #     RVAL$conf.int <- pp/(1 - pp) * T[2L]/T[1L]
  #     names(r) <- "rate ratio"
  #     RVAL$null.value <- r
  #     RVAL$method <- "Comparison of Poisson rates"
  #     return(RVAL)
  #   }
  #   else {
  #     m <- r * T
  #     PVAL <- switch(alternative, less = ppois(x, m), greater = ppois(x - 
  #                                                                       1, m, lower.tail = FALSE), two.sided = {
  #                                                                         if (m == 0) (x == 0) else {
  #                                                                           relErr <- 1 + 1e-07
  #                                                                           d <- dpois(x, r * T)
  #                                                                           if (x == m) 1 else if (x < m) {
  #                                                                             N <- ceiling(2 * m - x)
  #                                                                             while (dpois(N, m) > d) N <- 2 * N
  #                                                                             i <- seq.int(from = ceiling(m), to = N)
  #                                                                             y <- sum(dpois(i, m) <= d * relErr)
  #                                                                             ppois(x, m) + ppois(N - y, m, lower.tail = FALSE)
  #                                                                           } else {
  #                                                                             i <- seq.int(from = 0, to = floor(m))
  #                                                                             y <- sum(dpois(i, m) <= d * relErr)
  #                                                                             ppois(y - 1, m) + ppois(x - 1, m, lower.tail = FALSE)
  #                                                                           }
  #                                                                         }
  #                                                                       })
  #     p.L <- function(x, alpha) {
  #       if (x == 0) 
  #         0
  #       else qgamma(alpha, x)
  #     }
  #     p.U <- function(x, alpha) qgamma(1 - alpha, x + 1)
  #     CINT <- switch(alternative, less = c(0, p.U(x, 1 - conf.level)), 
  #                    greater = c(p.L(x, 1 - conf.level), Inf), two.sided = {
  #                      alpha <- (1 - conf.level)/2
  #                      c(p.L(x, alpha), p.U(x, alpha))
  #                    })/T
  #     attr(CINT, "conf.level") <- conf.level
  #     ESTIMATE <- x/T
  #     names(x) <- "number of events"
  #     names(T) <- "time base"
  #     names(ESTIMATE) <- names(r) <- "event rate"
  #     structure(list(statistic = x, parameter = T, p.value = PVAL, 
  #                    conf.int = CINT, estimate = ESTIMATE, null.value = r, 
  #                    alternative = alternative, method = "Exact Poisson test", 
  #                    data.name = DNAME), class = "htest")
  #   }
  # }
  # 
  
  
}