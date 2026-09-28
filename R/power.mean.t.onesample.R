# -----------------------------------------------------------------------------
# power.mean.t.onesample
# Bug: two.sided used only one t critical value.
# Fix: beta = P(t.lower < T < t.upper | ncp).
# -----------------------------------------------------------------------------
power.mean.t.onesample <- function(sample.size, effect.size, variance.est = 1,
                                   alpha = 0.05,
                                   alternative = c("two.sided", "less", "greater"),
                                   details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  se.est <- sqrt(variance.est)
  df <- sample.size - 1
  ncp <- effect.size / (se.est / sqrt(sample.size))
  t.upper <- qt(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), df = df, lower.tail = FALSE)
  t.lower <- qt(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), df = df, lower.tail = TRUE)

  if (alternative[1] == "two.sided") {
    beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE) -
      pt(t.lower, df = df, ncp = ncp, lower.tail = TRUE)
  } else if (effect.size < 0) {
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
      test = "t", type = "one.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size, df = df,
      effect.size = effect.size, variance = variance.est, alpha = alpha,
      conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
