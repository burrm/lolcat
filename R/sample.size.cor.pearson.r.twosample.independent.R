# -----------------------------------------------------------------------------
# sample.size.cor.pearson.r.twosample.independent
#
# Equal sample size per group for power.cor.pearson.r.twosample.independent().
# With n12 = n34 = n, the Fisher (1921) z difference has standard error
# sqrt(2/(n-3)), so
#   n = 2 * ((z_alpha + z_beta) / (z12 - z34))^2 + 3
# per group. sample.size.g1.g2 and sample.size.g3.g4 are that n, rounded up.
# actual is the formula before rounding. "greater" requires rho12 > rho34;
# "less" requires rho12 < rho34; otherwise NA.
#
# Fisher, R. A. (1921). On the "probable error" of a coefficient of correlation
#   deduced from a small sample. Metron, 1, 3-32.
# Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
#   (2nd ed.). Erlbaum. Chapter 4.
# -----------------------------------------------------------------------------
sample.size.cor.pearson.r.twosample.independent <- function(correlation.g1.g2,
                                                            correlation.g3.g4,
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
  if (length(correlation.g1.g2) != 1 || !is.finite(correlation.g1.g2) ||
      correlation.g1.g2 <= -1 || correlation.g1.g2 >= 1) {
    stop("correlation.g1.g2 must be a single correlation strictly between -1 and 1")
  }
  if (length(correlation.g3.g4) != 1 || !is.finite(correlation.g3.g4) ||
      correlation.g3.g4 <= -1 || correlation.g3.g4 >= 1) {
    stop("correlation.g3.g4 must be a single correlation strictly between -1 and 1")
  }

  beta.requested <- beta
  z_r12 <- 0.5 * log((1 + correlation.g1.g2) / (1 - correlation.g1.g2))
  z_r34 <- 0.5 * log((1 + correlation.g3.g4) / (1 - correlation.g3.g4))
  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z_beta <- qnorm(beta, lower.tail = FALSE)
  n <- 2 * ((z_alpha + z_beta) / (z_r12 - z_r34))^2 + 3

  if (alternative[1] == "greater" && z_r12 <= z_r34) {
    n <- NA
  }
  if (alternative[1] == "less" && z_r12 >= z_r34) {
    n <- NA
  }
  if (alternative[1] == "two.sided" && abs(z_r12 - z_r34) < 1e-12) {
    n <- NA
  }

  if (!is.finite(n)) {
    n.rounded <- NA_real_
    power <- NA_real_
    beta <- NA_real_
  } else {
    n.rounded <- max(4, ceiling(n))
    if (power.from.actual) {
      power <- 1 - beta.requested
      beta <- beta.requested
    } else {
      power <- power.cor.pearson.r.twosample.independent(
        sample.size.g1.g2 = n.rounded,
        sample.size.g3.g4 = n.rounded,
        correlation.g1.g2 = correlation.g1.g2,
        correlation.g3.g4 = correlation.g3.g4,
        alpha = alpha,
        alternative = alternative,
        details = FALSE
      )
      beta <- 1 - power
    }
  }

  if (details) {
    as.data.frame(list(
      test = "z", type = "cor.pearson.r.twosample.independent", alternative = alternative[1],
      sample.size.g1.g2 = n.rounded, sample.size.g3.g4 = n.rounded, actual = n,
      effect.size = correlation.g1.g2 - correlation.g3.g4,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = power
    ))
  } else {
    n.rounded
  }
}
