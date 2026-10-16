# -----------------------------------------------------------------------------
# power.cor.pearson.r.twosample.independent
#
# Power of the two-sample independent Pearson test
# cor.pearson.r.twosample.independent(): H0 is rho(g1,g2) = rho(g3,g4).
# Each sample is n i.i.d. bivariate normal pairs. Fisher's z,
#   z(r) = (1/2) log((1+r)/(1-r)),
# is treated as normal with variance 1/(n-3). The test statistic is
#   Z = (z12 - z34) / sqrt(1/(n12-3) + 1/(n34-3)).
# "greater" rejects for large Z (rho12 > rho34). "less" rejects for small Z.
# Two-sided power uses both normal tails. At rho12 = rho34, power = alpha.
#
# Fisher, R. A. (1921). On the "probable error" of a coefficient of correlation
#   deduced from a small sample. Metron, 1, 3-32.
# Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
#   (2nd ed.). Erlbaum. Chapter 4, the difference between two correlations.
# -----------------------------------------------------------------------------
power.cor.pearson.r.twosample.independent <- function(sample.size.g1.g2,
                                                      sample.size.g3.g4,
                                                      correlation.g1.g2,
                                                      correlation.g3.g4,
                                                      alpha = 0.05,
                                                      alternative = c("two.sided", "less", "greater"),
                                                      details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  if (length(sample.size.g1.g2) != 1 || !is.finite(sample.size.g1.g2) || sample.size.g1.g2 <= 3) {
    stop("sample.size.g1.g2 must be a single number greater than 3")
  }
  if (length(sample.size.g3.g4) != 1 || !is.finite(sample.size.g3.g4) || sample.size.g3.g4 <= 3) {
    stop("sample.size.g3.g4 must be a single number greater than 3")
  }
  if (length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number strictly between 0 and 1")
  }
  if (length(correlation.g1.g2) != 1 || !is.finite(correlation.g1.g2) ||
      correlation.g1.g2 <= -1 || correlation.g1.g2 >= 1) {
    stop("correlation.g1.g2 must be a single correlation strictly between -1 and 1")
  }
  if (length(correlation.g3.g4) != 1 || !is.finite(correlation.g3.g4) ||
      correlation.g3.g4 <= -1 || correlation.g3.g4 >= 1) {
    stop("correlation.g3.g4 must be a single correlation strictly between -1 and 1")
  }

  z_r12 <- 0.5 * log((1 + correlation.g1.g2) / (1 - correlation.g1.g2))
  z_r34 <- 0.5 * log((1 + correlation.g3.g4) / (1 - correlation.g3.g4))
  se <- sqrt(1 / (sample.size.g1.g2 - 3) + 1 / (sample.size.g3.g4 - 3))
  # Same contrast as cor.pearson.r.twosample.independent.simple(): z12 - z34.
  ncp <- (z_r12 - z_r34) / se
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)

  if (alternative[1] == "two.sided") {
    beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE) -
      pnorm(z.lower, mean = ncp, sd = 1, lower.tail = TRUE)
  } else if (alternative[1] == "greater") {
    beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE)
  } else {
    beta <- pnorm(z.lower, mean = ncp, sd = 1, lower.tail = FALSE)
  }
  pow <- 1 - beta

  if (details) {
    as.data.frame(list(
      test = "z", type = "cor.pearson.r.twosample.independent", alternative = alternative[1],
      sample.size.g1.g2 = sample.size.g1.g2, sample.size.g3.g4 = sample.size.g3.g4,
      effect.size = correlation.g1.g2 - correlation.g3.g4,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
