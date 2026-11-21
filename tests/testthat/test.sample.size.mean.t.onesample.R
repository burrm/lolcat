context("One-sample t test sample size alternative")

test_that("default and explicit alternative vectors use their first element", {
  for (effect in c(2, -2)) {
    scalar <- sample.size.mean.t.onesample(
      effect.size = effect, variance.est = 1, alpha = .05, beta = .1,
      alternative = "two.sided")
    expect_equal(sample.size.mean.t.onesample(
      effect.size = effect, variance.est = 1, alpha = .05, beta = .1), scalar)
    expect_equal(sample.size.mean.t.onesample(
      effect.size = effect, variance.est = 1, alpha = .05, beta = .1,
      alternative = c("two.sided", "less", "greater")), scalar)
    expect_equal(scalar$alternative, "two.sided")
    expect_true(is.finite(scalar$sample.size))
    expect_gte(scalar$power, .9)
  }
})

test_that("one-sided vectors preserve direction checks and scalar results", {
  for (direction in c("less", "greater")) {
    choices <- c(direction, setdiff(c("two.sided", "less", "greater"), direction))
    for (effect in c(-2, 2)) {
      for (details in c(TRUE, FALSE)) {
        scalar <- sample.size.mean.t.onesample(
          effect.size = effect, alternative = direction, details = details)
        vector <- sample.size.mean.t.onesample(
          effect.size = effect, alternative = choices, details = details)
        expect_equal(vector, scalar)
        n <- if (details) vector$sample.size else vector
        wrong.direction <- (effect > 0 && direction == "less") ||
          (effect < 0 && direction == "greater")
        if (wrong.direction) {
          expect_true(is.na(n))
        } else {
          expect_true(is.finite(n))
        }
        if (details) expect_equal(vector$alternative, direction)
      }
    }
  }
})

test_that("reported sample size is the first integer meeting the power target", {
  for (direction in c("two.sided", "less", "greater")) {
    effect <- if (direction == "less") -2 else 2
    result <- sample.size.mean.t.onesample(
      effect.size = effect, variance.est = 1, alpha = .05, beta = .1,
      alternative = c(direction, setdiff(c("two.sided", "less", "greater"), direction)))
    # Calculate power directly from the noncentral t distribution.
    power.at <- function(n) {
      df <- n - 1
      ncp <- effect * sqrt(n)
      if (direction == "two.sided") {
        lower <- qt(.025, df)
        upper <- qt(.975, df)
        pt(lower, df, ncp = ncp) + pt(upper, df, ncp = ncp, lower.tail = FALSE)
      } else if (direction == "less") {
        pt(qt(.05, df), df, ncp = ncp)
      } else {
        pt(qt(.95, df), df, ncp = ncp, lower.tail = FALSE)
      }
    }
    expect_equal(result$power, power.at(result$sample.size))
    expect_gte(power.at(result$sample.size), .9)
    expect_lt(power.at(result$sample.size - 1), .9)
    expect_equal(sample.size.mean.t.onesample(
      effect.size = effect, alternative = direction, details = FALSE), result$sample.size)
  }
})
