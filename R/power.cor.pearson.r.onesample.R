# -----------------------------------------------------------------------------
# power.cor.pearson.r.onesample  (Fisher z)
# Bug: two.sided used only one z critical value.
# Fix: both limbs via noncentral (shifted) normal.
# -----------------------------------------------------------------------------
power.cor.pearson.r.onesample <- function(sample.size,
                                          null.hypothesis.correlation,
                                          alternative.hypothesis.correlation,
                                          alpha = 0.05,
                                          alternative = c("two.sided", "less", "greater"),
                                          details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  z_r.null <- 0.5 * log((1 + null.hypothesis.correlation) /
                          (1 - null.hypothesis.correlation))
  z_r.alternative <- 0.5 * log((1 + alternative.hypothesis.correlation) /
                                 (1 - alternative.hypothesis.correlation))
  se <- sqrt(1 / (sample.size - 3))
  ncp <- (z_r.alternative - z_r.null) / se
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)

  if (alternative[1] == "two.sided") {
    beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE) -
      pnorm(z.lower, mean = ncp, sd = 1, lower.tail = TRUE)
  } else if (ncp < 0) {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.lower, mean = ncp, sd = 1, lower.tail = FALSE)
    }
  } else {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.lower, mean = ncp, sd = 1, lower.tail = FALSE)
    }
  }
  pow <- 1 - beta
  if (details) {
    as.data.frame(list(
      test = "z", type = "cor.pearson.r.onesample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      effect.size = alternative.hypothesis.correlation - null.hypothesis.correlation,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
