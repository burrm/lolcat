# -----------------------------------------------------------------------------
# power.proportion.test.onesample.approximate
# Bug: two.sided uses Phi(z_beta) with z_alpha = z_{alpha/2} — one limb only,
#      and mismatched one-sided alternatives return NA.
#
# Changed (vs an earlier both-limbs draft that kept lolcat's mixed SE):
#   lolcat 2.0.1 uses a *mixed* SE for the one-limb formula — critical value
#   with sqrt(pi0*(1-pi0)), powering with sqrt(pi1*(1-pi1)). Extending that
#   mixed-SE formula to both limbs does *not* match the usual score / H0-
#   variance z-test (or stats4ROI). For two.sided we therefore use NCP under
#   H0 SE only:
#     ncp = |pi1 - pi0| / sqrt(pi0*(1-pi0)/n)
#     power = 1 - (Phi(z_{1-a/2}; ncp) - Phi(z_{a/2}; ncp))
#   One-sided branches still follow lolcat's mixed-SE Phi(z_beta) form.
# -----------------------------------------------------------------------------
power.proportion.test.onesample.approximate <- function(
    sample.size, null.hypothesis.proportion, alternative.hypothesis.proportion,
    alpha = 0.05, alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  n <- sample.size
  p0 <- null.hypothesis.proportion
  p1 <- alternative.hypothesis.proportion
  se.null <- p0 * (1 - p0)
  se.alternative <- p1 * (1 - p1)
  se_null <- sqrt(se.null)
  se_alt <- sqrt(se.alternative)
  mu <- sqrt(n) * abs(p0 - p1)

  if (alternative[1] == "two.sided") {
    # Score-test / H0-SE both limbs (not lolcat's mixed-SE one-limb extension)
    ncp <- abs(p1 - p0) / sqrt(se.null / n)
    zl <- qnorm(alpha / 2)
    zu <- qnorm(1 - alpha / 2)
    beta <- pnorm(zu, mean = ncp, sd = 1, lower.tail = TRUE) -
      pnorm(zl, mean = ncp, sd = 1, lower.tail = TRUE)
    pow <- 1 - beta
  } else {
    # lolcat mixed-SE one-sided form (cutoff under H0 SE, power under H1 SE)
    z_alpha <- qnorm(alpha, lower.tail = FALSE)
    z_beta <- (mu - z_alpha * se_null) / se_alt
    pow <- pnorm(z_beta, lower.tail = TRUE)
    if (alternative[1] == "less" && p1 > p0) {
      pow <- NA
    } else if (alternative[1] == "greater" && p1 < p0) {
      pow <- NA
    }
  }
  if (details) {
    as.data.frame(list(
      test = "proportion", type = "one.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      null.hypothesis.proportion = p0,
      alternative.hypothesis.proportion = p1,
      alpha = alpha, conf.level = 1 - alpha, beta = 1 - pow, power = pow
    ))
  } else {
    pow
  }
}
