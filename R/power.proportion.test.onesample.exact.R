# -----------------------------------------------------------------------------
# power.proportion.test.onesample.exact
# Bug: two.sided only evaluates the binomial tail in the direction of pi1.
# Fix: both critical values under H0, both tails under H1 (lolcat cv conventions:
#      upper uses X > cv_u; lower uses X < cv_l).
# -----------------------------------------------------------------------------
power.proportion.test.onesample.exact <- function(
    sample.size, null.hypothesis.proportion, alternative.hypothesis.proportion,
    alpha = 0.05, alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  n <- sample.size
  p0 <- null.hypothesis.proportion
  p1 <- alternative.hypothesis.proportion

  if (alternative[1] == "two.sided") {
    cv_u <- qbinom(alpha / 2, n, p0, lower.tail = FALSE)
    cv_l <- qbinom(alpha / 2, n, p0, lower.tail = TRUE)
    pow <- pbinom(cv_u, n, p1, lower.tail = FALSE) +
      pbinom(cv_l, n, p1, lower.tail = TRUE) - dbinom(cv_l, n, p1)
  } else if (p0 < p1) {
    cv <- qbinom(alpha, n, p0, lower.tail = FALSE)
    pow <- pbinom(cv, n, p1, lower.tail = FALSE)
  } else {
    cv <- qbinom(alpha, n, p0, lower.tail = TRUE)
    pow <- pbinom(cv, n, p1, lower.tail = TRUE) - dbinom(cv, n, p1)
  }
  if (alternative[1] == "less" && p1 > p0) {
    pow <- NA
  } else if (alternative[1] == "greater" && p1 < p0) {
    pow <- NA
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
