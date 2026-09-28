# -----------------------------------------------------------------------------
# power.mean.z.onesample
# Bug: two.sided beta used only one critical bound (z.lower or z.upper).
# Fix: beta = P(z.lower < X < z.upper | effect.size).
# -----------------------------------------------------------------------------
power.mean.z.onesample <- function(sample.size, effect.size, variance = 1,
                                   alpha = 0.05,
                                   alternative = c("two.sided", "less", "greater"),
                                   details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  se.est <- sqrt(variance)
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)
  z.upper <- z.upper * se.est / sqrt(sample.size)
  z.lower <- z.lower * se.est / sqrt(sample.size)
  sd.x <- se.est / sqrt(sample.size)

  if (alternative[1] == "two.sided") {
    beta <- pnorm(z.upper, mean = effect.size, sd = sd.x, lower.tail = TRUE) -
      pnorm(z.lower, mean = effect.size, sd = sd.x, lower.tail = TRUE)
  } else if (effect.size < 0) {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper, mean = effect.size, sd = sd.x, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.lower, mean = effect.size, sd = sd.x, lower.tail = FALSE)
    }
  } else {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper, mean = effect.size, sd = sd.x, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.lower, mean = effect.size, sd = sd.x, lower.tail = FALSE)
    }
  }
  pow <- 1 - beta
  if (details) {
    as.data.frame(list(
      test = "z", type = "one.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      effect.size = effect.size, variance = variance, alpha = alpha,
      conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
