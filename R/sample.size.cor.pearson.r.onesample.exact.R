# -----------------------------------------------------------------------------
# sample.size.cor.pearson.r.onesample.exact
#
# Smallest integer n > 3 whose exact one-sample Pearson power is at least
# 1 - beta. Power is power.cor.pearson.r.onesample.exact(). The Fisher (1921)
# z sample size is only where the integer search starts.
#
# A greater alternative with rho1 <= rho0, a less alternative with
# rho1 >= rho0, or a two-sided test with rho1 = rho0 returns NA, matching
# sample.size.cor.pearson.r.onesample() on a wrong-direction alternative.
#
# Fisher, R. A. (1915). Frequency distribution of the values of the correlation
#   coefficient in samples from an indefinitely large population. Biometrika,
#   10(4), 507-521.
# Fisher, R. A. (1921). On the "probable error" of a coefficient of correlation
#   deduced from a small sample. Metron, 1, 3-32.
# Barabesi, L., & Greco, L. (2002). A note on the exact computation of the
#   Student t, Snedecor F and sample correlation coefficient distribution
#   functions. The Statistician, 51(1), 105-110.
# Faul, F., Erdfelder, E., Buchner, A., & Lang, A.-G. (2009). Statistical power
#   analyses using G*Power 3.1. Behavior Research Methods, 41, 1149-1160.
#   G*Power 3.1 manual, section 3.
# -----------------------------------------------------------------------------
sample.size.cor.pearson.r.onesample.exact <- function(null.hypothesis.correlation,
                                                      alternative.hypothesis.correlation,
                                                      alpha = 0.05,
                                                      beta = 0.1,
                                                      alternative = c("two.sided", "less", "greater"),
                                                      details = TRUE,
                                                      power.from.actual = F) {
  validate.htest.alternative(alternative = alternative)
  if (length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number strictly between 0 and 1")
  }
  if (length(beta) != 1 || !is.finite(beta) || beta <= 0 || beta >= 1) {
    stop("beta must be a single number strictly between 0 and 1")
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

  z_r.null <- 0.5 * log((1 + null.hypothesis.correlation) /
                          (1 - null.hypothesis.correlation))
  z_r.alternative <- 0.5 * log((1 + alternative.hypothesis.correlation) /
                                 (1 - alternative.hypothesis.correlation))
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z_beta <- qnorm(beta, lower.tail = FALSE)
  n <- ((z_alpha + z_beta) / (z_r.alternative - z_r.null))^2 + 3

  if (alternative[1] == "greater" && z_r.alternative <= z_r.null) {
    n <- NA
  }
  if (alternative[1] == "less" && z_r.alternative >= z_r.null) {
    n <- NA
  }
  if (alternative[1] == "two.sided" && abs(z_r.alternative - z_r.null) < 1e-12) {
    n <- NA
  }

  max.n <- 100000
  n.rounded <- NA_real_
  if (is.finite(n) && n <= max.n) {
    target <- 1 - beta
    n.try <- max(4, as.integer(ceiling(n)))
    pow.n <- power.cor.pearson.r.onesample.exact(
      sample.size = n.try,
      null.hypothesis.correlation = null.hypothesis.correlation,
      alternative.hypothesis.correlation = alternative.hypothesis.correlation,
      alpha = alpha,
      alternative = alternative,
      details = FALSE
    )
    if (pow.n >= target) {
      while (n.try > 4) {
        pow.prev <- power.cor.pearson.r.onesample.exact(
          sample.size = n.try - 1,
          null.hypothesis.correlation = null.hypothesis.correlation,
          alternative.hypothesis.correlation = alternative.hypothesis.correlation,
          alpha = alpha,
          alternative = alternative,
          details = FALSE
        )
        if (pow.prev < target) {
          break
        }
        n.try <- n.try - 1
      }
      n.rounded <- n.try
    } else {
      while (n.try < max.n && pow.n < target) {
        n.try <- n.try + 1
        pow.n <- power.cor.pearson.r.onesample.exact(
          sample.size = n.try,
          null.hypothesis.correlation = null.hypothesis.correlation,
          alternative.hypothesis.correlation = alternative.hypothesis.correlation,
          alpha = alpha,
          alternative = alternative,
          details = FALSE
        )
      }
      if (pow.n >= target) {
        n.rounded <- n.try
      }
    }
  }

  beta.requested <- beta
  if (is.na(n.rounded)) {
    power <- NA_real_
    beta <- NA_real_
    crit.lower <- NA_real_
    crit.upper <- NA_real_
  } else {
    out <- power.cor.pearson.r.onesample.exact(
      sample.size = n.rounded,
      null.hypothesis.correlation = null.hypothesis.correlation,
      alternative.hypothesis.correlation = alternative.hypothesis.correlation,
      alpha = alpha,
      alternative = alternative,
      details = TRUE
    )
    power <- out$power
    beta <- out$beta
    crit.lower <- out$critical.r.lower
    crit.upper <- out$critical.r.upper
    if (power.from.actual) {
      power <- 1 - beta.requested
      beta <- beta.requested
    }
  }

  if (details) {
    as.data.frame(list(
      test = "exact", type = "cor.pearson.r.onesample.exact", alternative = alternative[1],
      sample.size = n.rounded, actual = n.rounded,
      effect.size = alternative.hypothesis.correlation - null.hypothesis.correlation,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = power,
      critical.r.lower = crit.lower, critical.r.upper = crit.upper
    ))
  } else {
    n.rounded
  }
}
