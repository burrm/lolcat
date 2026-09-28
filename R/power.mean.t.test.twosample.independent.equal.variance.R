# -----------------------------------------------------------------------------
# power.mean.t.test.twosample.independent.equal.variance
# Same two-sided pattern as one-sample t.
# -----------------------------------------------------------------------------
power.mean.t.test.twosample.independent.equal.variance <- function(
    mean.g1 = 0, mean.g2 = 1,
    variance.est.g1 = 1, variance.est.g2 = variance.est.g1,
    sample.size.g1 = 2, sample.size.g2 = sample.size.g1,
    null.hypothesis.difference = 0, alpha = 0.05,
    alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  se.est <- sqrt((variance.est.g1 + variance.est.g2) / 2)
  d <- ((mean.g2 - mean.g1) - null.hypothesis.difference) / se.est
  df <- sample.size.g1 + sample.size.g2 - 2
  ncp <- d * sqrt(sample.size.g1 * sample.size.g2 / (sample.size.g1 + sample.size.g2))
  t.upper <- qt(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), df = df, lower.tail = FALSE)
  t.lower <- qt(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), df = df, lower.tail = TRUE)

  if (alternative[1] == "two.sided") {
    beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE) -
      pt(t.lower, df = df, ncp = ncp, lower.tail = TRUE)
  } else if (d < 0) {
    if (alternative[1] == "greater") {
      beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE)
    } else {
      beta <- pt(t.lower, df = df, ncp = ncp, lower.tail = FALSE)
    }
  } else {
    if (alternative[1] == "greater") {
      beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE)
    } else {
      beta <- pt(t.lower, df = df, ncp = ncp, lower.tail = FALSE)
    }
  }
  pow <- 1 - beta
  if (details) {
    as.data.frame(list(
      test = "t", type = "two.sample", alternative = alternative[1],
      mean.diff = mean.g2 - mean.g1,
      sample.size.g1 = sample.size.g1, sample.size.g2 = sample.size.g2,
      df = df, alpha = alpha, conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
