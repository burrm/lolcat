# -----------------------------------------------------------------------------
# power.mean.t.onesample
# Bug (2.0.1): two.sided used only one t critical value.
# Fix: beta = P(t.lower < T < t.upper | ncp).
# Also: evaluate pt via sign(ncp) form so large |ncp| does not trigger pnt{final}.
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
    # F(t|ncp) = 1 - F(-t|-ncp); choose form so both CDFs stay away from 1
    if (ncp >= 0) {
      beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE) -
        pt(t.lower, df = df, ncp = ncp, lower.tail = TRUE)
    } else {
      beta <- pt(-t.lower, df = df, ncp = -ncp, lower.tail = TRUE) -
        pt(-t.upper, df = df, ncp = -ncp, lower.tail = TRUE)
    }
  } else if (alternative[1] == "greater") {
    if (ncp >= 0) {
      beta <- pt(t.upper, df = df, ncp = ncp, lower.tail = TRUE)
    } else {
      beta <- 1 - pt(-t.upper, df = df, ncp = -ncp, lower.tail = TRUE)
    }
  } else {
    # alternative == "less": P(T > t.lower | ncp) without lower.tail = FALSE
    if (ncp <= 0) {
      beta <- pt(-t.lower, df = df, ncp = -ncp, lower.tail = TRUE)
    } else {
      beta <- 1 - pt(t.lower, df = df, ncp = ncp, lower.tail = TRUE)
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
