# -----------------------------------------------------------------------------
# power.cor.pearson.r.onesample.exact
#
# Exact power of H0: rho = rho0 for n i.i.d. bivariate normal pairs.
# Critical values are quantiles of Fisher's (1915) density of the sample
# correlation under rho0. Two-sided critical values are equal-tailed on the
# probability scale, so they are not symmetric about rho0 when rho0 != 0.
# Power is the probability that R falls in that rejection region at rho1.
# When rho0 = 0 the critical value is the Student-t critical value on the r
# scale. This is not a noncentral-t probability.
#
# G*Power's exact "Correlation: bivariate normal model" is this probability;
# it evaluates the CDF with Barabesi and Greco (2002). Checked against the
# G*Power 3.1 manual, section 3.3: n = 8, rho0 = 0.8, rho1 = 0.3, two-sided
# alpha = 0.05, power = 0.482927.
#
# Fisher, R. A. (1915). Frequency distribution of the values of the correlation
#   coefficient in samples from an indefinitely large population. Biometrika,
#   10(4), 507-521.
# Barabesi, L., & Greco, L. (2002). A note on the exact computation of the
#   Student t, Snedecor F and sample correlation coefficient distribution
#   functions. The Statistician, 51(1), 105-110.
# Faul, F., Erdfelder, E., Buchner, A., & Lang, A.-G. (2009). Statistical power
#   analyses using G*Power 3.1. Behavior Research Methods, 41, 1149-1160.
#   G*Power 3.1 manual, section 3.
# -----------------------------------------------------------------------------
power.cor.pearson.r.onesample.exact <- function(sample.size,
                                                null.hypothesis.correlation,
                                                alternative.hypothesis.correlation,
                                                alpha = 0.05,
                                                alternative = c("two.sided", "less", "greater"),
                                                details = TRUE) {
  validate.htest.alternative(alternative = alternative)

  # 2F1(1/2, 1/2; n - 1/2; z), the series factor in Fisher's density.
  hyper2f1.half <- function(z, n, tol = 1e-14, max.k = 2000L) {
    cpar <- n - 0.5
    term <- rep(1, length(z))
    s <- term
    active <- rep(TRUE, length(z))
    for (k in seq_len(max.k)) {
      term[active] <- term[active] * ((k - 0.5)^2) / (cpar + k - 1) / k * z[active]
      s[active] <- s[active] + term[active]
      active <- active & (abs(term) >= tol * pmax(abs(s), 1e-300))
      if (!any(active)) {
        break
      }
    }
    s
  }

  # Fisher (1915) density of the sample correlation.
  density <- function(r, rho, n) {
    r <- pmin(pmax(r, -1 + 1e-15), 1 - 1e-15)
    z <- 0.5 * (1 + rho * r)
    log.f <- log(n - 2) + lgamma(n - 1) - 0.5 * log(2 * pi) - lgamma(n - 0.5) +
      ((n - 1) / 2) * log(1 - rho^2) +
      ((n - 4) / 2) * log(1 - r^2) -
      (n - 1.5) * log(1 - rho * r) +
      log(hyper2f1.half(z, n))
    exp(log.f)
  }

  # P(R <= q) or P(R >= q). rho = 0 uses the beta form of the same density.
  tail.prob <- function(q, rho, n, lower.tail = TRUE) {
    if (!is.finite(q)) {
      stop("critical value is not finite")
    }
    if (lower.tail) {
      if (q <= -1) {
        return(0)
      }
      if (q >= 1) {
        return(1)
      }
    } else {
      if (q >= 1) {
        return(0)
      }
      if (q <= -1) {
        return(1)
      }
    }
    if (abs(rho) <= 1e-15) {
      return(pbeta((1 + q) / 2, (n - 2) / 2, (n - 2) / 2, lower.tail = lower.tail))
    }
    eps <- 1e-12
    if (lower.tail) {
      if (q <= -1 + eps) {
        return(0)
      }
      val <- integrate(
        function(r) density(r, rho, n),
        lower = -1 + eps, upper = q,
        rel.tol = 1e-8, abs.tol = 1e-10, subdivisions = 500L
      )$value
    } else {
      if (q >= 1 - eps) {
        return(0)
      }
      val <- integrate(
        function(r) density(r, rho, n),
        lower = q, upper = 1 - eps,
        rel.tol = 1e-8, abs.tol = 1e-10, subdivisions = 500L
      )$value
    }
    min(max(val, 0), 1)
  }

  quantile.r <- function(p, rho, n) {
    if (p <= 0) {
      return(-1)
    }
    if (p >= 1) {
      return(1)
    }
    if (abs(rho) <= 1e-15) {
      return(2 * qbeta(p, (n - 2) / 2, (n - 2) / 2) - 1)
    }
    f <- function(q) tail.prob(q, rho, n, lower.tail = TRUE) - p
    lo <- -1 + 1e-8
    hi <- 1 - 1e-8
    if (f(lo) >= 0) {
      return(lo)
    }
    if (f(hi) <= 0) {
      return(hi)
    }
    uniroot(f, c(lo, hi), tol = 1e-8)$root
  }

  critical.r <- function(n, rho0, alpha, alternative) {
    if (alternative == "two.sided") {
      return(list(
        lower = quantile.r(alpha / 2, rho0, n),
        upper = quantile.r(1 - alpha / 2, rho0, n)
      ))
    }
    if (alternative == "greater") {
      return(list(
        lower = NA_real_,
        upper = quantile.r(1 - alpha, rho0, n)
      ))
    }
    list(
      lower = quantile.r(alpha, rho0, n),
      upper = NA_real_
    )
  }

  if (length(sample.size) != 1 || !is.finite(sample.size) || sample.size <= 3) {
    stop("sample.size must be a single number greater than 3")
  }
  if (length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number strictly between 0 and 1")
  }
  if (length(null.hypothesis.correlation) != 1 ||
      !is.finite(null.hypothesis.correlation) ||
      null.hypothesis.correlation <= -1 || null.hypothesis.correlation >= 1) {
    stop("null.hypothesis.correlation must be a single correlation strictly between -1 and 1")
  }
  if (length(alternative.hypothesis.correlation) != 1 ||
      !is.finite(alternative.hypothesis.correlation) ||
      alternative.hypothesis.correlation <= -1 || alternative.hypothesis.correlation >= 1) {
    stop("alternative.hypothesis.correlation must be a single correlation strictly between -1 and 1")
  }

  crit <- critical.r(
    sample.size,
    null.hypothesis.correlation,
    alpha,
    alternative[1]
  )
  if (alternative[1] == "two.sided") {
    pow <- tail.prob(crit$lower, alternative.hypothesis.correlation, sample.size, TRUE) +
      tail.prob(crit$upper, alternative.hypothesis.correlation, sample.size, FALSE)
  } else if (alternative[1] == "greater") {
    pow <- tail.prob(crit$upper, alternative.hypothesis.correlation, sample.size, FALSE)
  } else {
    pow <- tail.prob(crit$lower, alternative.hypothesis.correlation, sample.size, TRUE)
  }
  pow <- min(max(pow, 0), 1)
  beta <- 1 - pow

  if (details) {
    as.data.frame(list(
      test = "exact", type = "cor.pearson.r.onesample.exact", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      effect.size = alternative.hypothesis.correlation - null.hypothesis.correlation,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = pow,
      critical.r.lower = crit$lower, critical.r.upper = crit$upper
    ))
  } else {
    pow
  }
}
