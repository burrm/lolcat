# -----------------------------------------------------------------------------
# power.variance.onesample
# Bug: two.sided beta used only the critical chi-square on the "near" side of
#      the ratio. Both chi.lower and chi.upper are already computed.
# Fix: power = P(X <= chi.lower | H1) + P(X > chi.upper | H1)
#      under the scaled gamma used by lolcat (shape=(n-1)/2, scale=2*ratio).
# -----------------------------------------------------------------------------
power.variance.onesample <- function(sample.size = 1,
                                     null.hypothesis.variance = 1,
                                     alternative.hypothesis.variance = 2,
                                     alpha = 0.05,
                                     alternative = c("two.sided", "greater", "less"),
                                     details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  ratio <- alternative.hypothesis.variance / null.hypothesis.variance
  n <- sample.size
  if (alternative[1] == "two.sided") {
    chi.lower <- qchisq(alpha / 2, n - 1, lower.tail = TRUE)
    chi.upper <- qchisq(1 - alpha / 2, n - 1, lower.tail = TRUE)
  } else {
    chi.lower <- qchisq(alpha, n - 1, lower.tail = TRUE)
    chi.upper <- qchisq(1 - alpha, n - 1, lower.tail = TRUE)
  }
  p.fn <- function(q, lower.tail = TRUE) {
    pgamma(q = q, shape = (n - 1) / 2, scale = 2 * ratio, lower.tail = lower.tail)
  }

  if (ratio == 1) {
    beta <- 1
  } else if (alternative[1] == "two.sided") {
    # both limbs
    pow <- p.fn(chi.lower, lower.tail = TRUE) + p.fn(chi.upper, lower.tail = FALSE)
    beta <- 1 - pow
  } else if (ratio < 1) {
    if (alternative[1] == "less") {
      beta <- p.fn(chi.lower, lower.tail = FALSE)
    } else {
      beta <- p.fn(chi.upper, lower.tail = TRUE)
    }
  } else {
    if (alternative[1] == "less") {
      beta <- p.fn(chi.lower, lower.tail = FALSE)
    } else {
      beta <- p.fn(chi.upper, lower.tail = TRUE)
    }
  }
  pow <- 1 - beta
  if (details) {
    as.data.frame(list(
      test = "chi-square", type = "one.sample", alternative = alternative[1],
      sample.size = n, df = n - 1, ratio = ratio, alpha = alpha,
      conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
