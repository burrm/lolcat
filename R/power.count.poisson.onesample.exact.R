# -----------------------------------------------------------------------------
# power.count.poisson.onesample.exact
# Bug: two.sided recursively calls one-sided at alpha/2 in only one direction.
# Fix: both Poisson critical counts under H0, sum both tails under H1.
#      (Uses qpois; equivalent in spirit to summing both one-sided limbs.)
# -----------------------------------------------------------------------------
power.count.poisson.onesample.exact <- function(
    sample.size, lambda.null.hypothesis, lambda.alternative.hypothesis,
    alpha = 0.05, alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  n <- sample.size
  lambda0 <- lambda.null.hypothesis
  lambda1 <- lambda.alternative.hypothesis
  pow <- NA
  alpha_out <- alpha

  if (alternative[1] == "two.sided") {
    if (lambda1 != lambda0) {
      crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda0, lower.tail = TRUE) - 1
      crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda0, lower.tail = TRUE) + 1
      alpha_out <- ppois(q = crit_x_l, lambda = n * lambda0, lower.tail = TRUE) +
        ppois(q = crit_x_u - 1, lambda = n * lambda0, lower.tail = FALSE)
      pow <- ppois(q = crit_x_l, lambda = n * lambda1, lower.tail = TRUE) +
        ppois(q = crit_x_u - 1, lambda = n * lambda1, lower.tail = FALSE)
    }
  } else if (alternative[1] == "greater") {
    if (lambda1 > lambda0) {
      table.lambda.alternative.hypothesis <- table.dist.poisson(lambda = n * lambda1)
      table.lambda.null.hypothesis <- table.dist.poisson(
        lambda = n * lambda0,
        include.x = nrow(table.lambda.alternative.hypothesis)
      )
      idx <- which(table.lambda.null.hypothesis$eq.and.above <= alpha)
      pow <- table.lambda.alternative.hypothesis$eq.and.above[min(idx) + 1]
      alpha_out <- table.lambda.null.hypothesis$eq.and.above[min(idx)]
    }
  } else if (alternative[1] == "less") {
    if (lambda1 < lambda0) {
      table.lambda.null.hypothesis <- table.dist.poisson(lambda = n * lambda0)
      table.lambda.alternative.hypothesis <- table.dist.poisson(
        lambda = n * lambda1,
        include.x = nrow(table.lambda.null.hypothesis)
      )
      idx <- which(table.lambda.null.hypothesis$eq.and.below <= alpha)
      alpha_out <- table.lambda.null.hypothesis$eq.and.below[length(idx)]
      pow <- table.lambda.alternative.hypothesis$eq.and.below[length(idx) - 1]
    }
  }
  beta <- 1 - pow
  if (details) {
    as.data.frame(list(
      test = "poisson", type = "one.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      lambda.null = lambda0, lambda.alternative = lambda1,
      alpha = alpha_out, conf.level = 1 - alpha_out, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
