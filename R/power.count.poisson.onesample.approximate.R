# -----------------------------------------------------------------------------
# power.count.poisson.onesample.approximate
# Bugs:
#   1) two.sided uses only one z critical value (same family as mean z).
#   2) one-sided branches swap lower.tail in places (lolcat 2.0.1); when
#      lambda_alt == lambda_null and alternative == "less", power ≈ 0.95
#      instead of ≈ alpha. Fixed one-sided tails below to match the intended
#      square-root Poisson z test.
# Fix two.sided: both standard-normal limbs with
#   z.beta = 2*sqrt(n)*(sqrt(lambda1)-sqrt(lambda0)),
#   power = 1 - (Phi(z_{1-a/2} - z.beta) - Phi(z_{a/2} - z.beta)).
# -----------------------------------------------------------------------------
power.count.poisson.onesample.approximate <- function(
    sample.size, lambda.null.hypothesis, lambda.alternative.hypothesis,
    alpha = 0.05, alternative = c("two.sided", "less", "greater"), details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)
  z.beta <- 2 * sqrt(sample.size) *
    (sqrt(lambda.alternative.hypothesis) - sqrt(lambda.null.hypothesis))

  if (alternative[1] == "two.sided") {
    zl <- qnorm(alpha / 2)
    zu <- qnorm(1 - alpha / 2)
    beta <- pnorm(zu, mean = z.beta, sd = 1, lower.tail = TRUE) -
      pnorm(zl, mean = z.beta, sd = 1, lower.tail = TRUE)
    pow <- 1 - beta
  } else if (lambda.alternative.hypothesis < lambda.null.hypothesis) {
    # lower-tailed test under decrease in rate
    if (alternative[1] == "greater") {
      beta <- pnorm(z.lower - z.beta, lower.tail = FALSE)
    } else {
      beta <- pnorm(z.lower - z.beta, lower.tail = FALSE)
    }
    pow <- 1 - beta
  } else {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper - z.beta, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.upper - z.beta, lower.tail = TRUE)
    }
    pow <- 1 - beta
  }
  if (details) {
    as.data.frame(list(
      test = "poisson", type = "one.sample", alternative = alternative[1],
      sample.size = sample.size, actual = sample.size,
      lambda.null = lambda.null.hypothesis,
      lambda.alternative = lambda.alternative.hypothesis,
      alpha = alpha, conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
