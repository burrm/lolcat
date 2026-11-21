# -----------------------------------------------------------------------------
# power.proportion.test.twosample.approximate
# Bug: same one-limb Phi(z_beta) pattern; mismatched one-sided → NA.
# Fix: both limbs with lolcat SE convention (se_null = sqrt(2*p1(1-p1)),
#      se_alt = sqrt(p1(1-p1)+p2(1-p2)), equal n per group).
# -----------------------------------------------------------------------------
power.proportion.test.twosample.approximate <- function(
    sample.size, proportion.g1, proportion.g2, alpha = 0.05,
    alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  n <- sample.size
  se.g1 <- proportion.g1 * (1 - proportion.g1)
  se.g2 <- proportion.g2 * (1 - proportion.g2)
  se_null <- sqrt(2 * se.g1)
  se_alt <- sqrt(se.g1 + se.g2)
  mu <- sqrt(n) * abs(proportion.g1 - proportion.g2)

  if (alternative[1] == "two.sided") {
    z_a <- qnorm(alpha / 2, lower.tail = FALSE)
    pow <- pnorm((mu - z_a * se_null) / se_alt) +
      pnorm((-z_a * se_null - mu) / se_alt)
  } else {
    z_alpha <- qnorm(alpha, lower.tail = FALSE)
    z_beta <- (mu - z_alpha * se_null) / se_alt
    pow <- pnorm(z_beta, lower.tail = TRUE)
    if (alternative[1] == "less" && proportion.g1 > proportion.g2) {
      pow <- NA
    } else if (alternative[1] == "greater" && proportion.g1 < proportion.g2) {
      pow <- NA
    }
  }
  if (details) {
    as.data.frame(list(
      test = "proportion", type = "two.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      proportion.g1 = proportion.g1, proportion.g2 = proportion.g2,
      alpha = alpha, conf.level = 1 - alpha, beta = 1 - pow, power = pow
    ))
  } else {
    pow
  }
}
