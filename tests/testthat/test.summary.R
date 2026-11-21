context("Summary functions")

test_that("each scalar statistic works on its own", {
  d <- data.frame(value = c(1, 2, 4, NA, 5, 5, 9, NA),
                  group = rep(c("a", "b"), each = 4))
  # Expected values come from base R definitions, independently of summary.impl.
  statistics <- list(
    n = function(x) sum(!is.na(x)),
    total.n = length,
    miss = function(x) sum(is.na(x)),
    sum = function(x) sum(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    var = function(x) var(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    mean.ADA = function(x) { x <- na.omit(x); mean(abs(x - mean(x))) },
    mean.ADM = function(x) { x <- na.omit(x); mean(abs(x - median(x))) },
    mean.ADMn1 = function(x) {
      x <- na.omit(x); deviations <- abs(x - median(x))
      mean(deviations[-which.min(deviations)])
    },
    min = function(x) min(x, na.rm = TRUE),
    q1 = function(x) unname(quantile(x, .25, na.rm = TRUE)),
    median = function(x) median(x, na.rm = TRUE),
    q3 = function(x) unname(quantile(x, .75, na.rm = TRUE)),
    max = function(x) max(x, na.rm = TRUE),
    range = function(x) diff(range(x, na.rm = TRUE)),
    iqr = function(x) IQR(x, na.rm = TRUE),
    psd = function(x) IQR(x, na.rm = TRUE) / 1.35,
    sir = function(x) IQR(x, na.rm = TRUE) / 2,
    coefvar = function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    distinct = function(x) length(unique(x[!is.na(x)])),
    distinct.withna = function(x) length(unique(x)),
    true.mode = function(x) 3 * median(x, na.rm = TRUE) - 2 * mean(x, na.rm = TRUE)
  )
  columns <- c(miss = "missing", q1 = "quartile.1", q3 = "quartile.3")
  for (stat in names(statistics)) {
    column <- if (stat %in% names(columns)) columns[[stat]] else stat
    args <- setNames(list(TRUE), paste0("stat.", stat))
    grouped <- do.call(summary.impl, c(list(value ~ group, data = d), args))
    expect_named(grouped, c("group", column), info = stat)
    expect_equal(grouped$group, c("a", "b"), info = stat)
    expect_equal(grouped[[column]],
                 unname(vapply(split(d$value, d$group), statistics[[stat]], numeric(1))),
                 info = stat)

    ungrouped <- do.call(summary.impl, c(list(value ~ 1, data = d), args))
    expect_named(ungrouped, c("dv.name", column), info = stat)
    expect_equal(ungrouped$dv.name, "value", info = stat)
    expect_equal(ungrouped[[column]], statistics[[stat]](d$value), info = stat)
  }
})

test_that("one group and multiple grouping variables preserve columns and order", {
  d <- data.frame(value = c(8, 2, 6, 4),
                  first = factor(c("b", "a", "b", "a")),
                  second = c(2, 2, 1, 1))
  result <- summary.impl(value ~ first + second, d, stat.mean = TRUE,
                         format.generate.cellcodes = TRUE)
  expect_named(result, c("first", "second", "cell.code", "mean"))
  expect_equal(as.character(result$first), c("a", "a", "b", "b"))
  expect_equal(result$second, c(1, 2, 1, 2))
  expect_equal(result$mean, c(4, 2, 6, 8))
  expect_equal(as.character(result$cell.code),
               c("Cell 1 - first a, second 1", "Cell 2 - first a, second 2",
                 "Cell 3 - first b, second 1", "Cell 4 - first b, second 2"))
  expect_s3_class(result$cell.code, "factor")

  one <- summary.impl(value ~ first, d[d$first == "a", ], stat.mean = TRUE,
                      format.generate.cellcodes = TRUE)
  expect_named(one, c("first", "cell.code", "mean"))
  expect_equal(one$mean, 3)
  expect_equal(as.character(one$cell.code), "Cell 1 - first a")
})

test_that("multiple statistics, quantiles and SD reports retain names and values", {
  d <- data.frame(value = c(1, 2, 4, 8, NA), group = "a")
  result <- summary.impl(value ~ group, d, stat.n = TRUE, stat.miss = TRUE,
                         stat.mean = TRUE, stat.var = TRUE, stat.five.number = TRUE,
                         stat.quantiles = c(.1, .9), stat.sd.report = c(-2, 2))
  expect_named(result, c("group", "n", "missing", "mean", "var", "min",
                         "quartile.1", "median", "quartile.3", "max",
                         "percentile.10", "percentile.90", "sd.x.m2", "sd.x.2"))
  x <- c(1, 2, 4, 8)
  expect_equal(as.numeric(result[1, -1]),
               c(4, 1, mean(x), var(x), unname(quantile(x, c(0, .25, .5, .75, 1))),
                 unname(quantile(x, c(.1, .9))), -2 * sd(x), 2 * sd(x)))
  expect_named(summary.impl(value ~ 1, d, stat.quantiles = .5),
               c("dv.name", "percentile.50"))
  expect_equal(summary.impl(value ~ 1, d, stat.sd.report = 2)$sd.x.2, 2 * sd(x))
})

test_that("missing values and all-missing groups have consistent counts", {
  d <- data.frame(value = c(1, NA, NA, NA, 99),
                  group = c("a", "a", "b", "b", NA))
  result <- summary.impl(value ~ group, d, stat.n = TRUE,
                         stat.total.n = TRUE, stat.miss = TRUE, stat.mean = TRUE)
  expect_equal(result$group, c("a", "b"))
  expect_equal(result$n, c(1, 0))
  expect_equal(result$total.n, c(2, 2))
  expect_equal(result$missing, c(1, 2))
  expect_equal(result$mean[1], 1)
  expect_true(is.nan(result$mean[2]))
  expect_equal(summary.impl(value ~ group, d, stat.n = TRUE)$n, c(1, 0))
  expect_equal(summary.impl(value ~ 1, d[3:4, ], stat.n = TRUE)$n, 0)
})

test_that("factor and character summaries support individual and combined statistics", {
  for (values in list(c("a", "a", "b", NA), factor(c("a", "a", "b", NA)))) {
    d <- data.frame(value = values, group = "g")
    expect_equal(summary.impl(value ~ group, d, stat.n = TRUE)$n, 3)
    expect_equal(summary.impl(value ~ group, d, stat.distinct = TRUE)$distinct, 2)
    expect_equal(summary.impl(value ~ group, d, stat.distinct.withna = TRUE)$distinct.withna, 3)
    result <- summary.impl(value ~ group, d, stat.n = TRUE, stat.miss = TRUE,
                           stat.mean = TRUE, stat.var = TRUE, stat.five.number = TRUE)
    expect_equal(result$n, 3)
    expect_equal(result$missing, 1)
    expect_true(all(is.na(result[c("mean", "var", "min", "quartile.1",
                                   "median", "quartile.3", "max")])))
  }
})

test_that("specification summaries count only observed out-of-limit values", {
  d <- data.frame(value = c(0, 1, 2, 3, 4, NA))
  result <- summary.impl(value ~ 1, d, stat.lsl = 1, stat.target = 2, stat.usl = 3,
                         stat.nonconform.nbelow = TRUE, stat.nonconform.nabove = TRUE,
                         stat.nonconform.nout = TRUE, stat.nonconform.pbelow = TRUE,
                         stat.nonconform.pabove = TRUE, stat.nonconform.pout = TRUE)
  expect_named(result, c("dv.name", "spec.lsl", "spec.tgt", "spec.usl", "spec.nbelow",
                         "spec.nabove", "spec.nout", "spec.pbelow", "spec.pabove", "spec.pout"))
  expect_equal(as.numeric(result[1, -1]), c(1, 2, 3, 1, 1, 2, .2, .2, .4))
})

test_that("continuous summaries apply defaults and forward statistic selections", {
  d <- data.frame(value = c(1, 2, 4, 8, NA, 3, 6, 9, 12, 15),
                  group = rep(c("a", "b"), each = 5))
  result <- summary.continuous(value ~ group, d)
  expect_named(result, c("group", "n", "missing", "mean", "var",
                         "adtest.AA", "adtest.p", "swtest.W", "swtest.p"))
  expect_equal(result$n, c(4, 5))
  expect_equal(result$missing, c(1, 0))
  expect_equal(result$mean, c(3.75, 9))
  expect_equal(result$var, c(var(c(1, 2, 4, 8)), var(c(3, 6, 9, 12, 15))))
  expect_true(all(is.finite(result$adtest.p)))
  expect_true(all(is.finite(result$swtest.p)))
  single <- summary.continuous(value ~ group, d, stat.n = FALSE, stat.miss = FALSE,
                               stat.var = FALSE, stat.ad.test = 0, stat.sw.test = 0,
                               stat.skew.test = 0, stat.kurt.test = 0)
  expect_named(single, c("group", "mean"))
  expect_equal(single$mean, c(3.75, 9))
})

test_that("shape decisions and sample-size gates are retained", {
  d <- data.frame(value = c(1:29, 100))
  result <- summary.impl(value ~ 1, d, stat.sw.test = TRUE,
                         stat.shape.rejection.conf.level = .95,
                         stat.shape.text.rej = "rejected", stat.shape.text.ftr = "retained")
  expect_equal(result$swtest.p, shapiro.wilk.normality.test(d$value)$p.value)
  expect_s3_class(result$swtest.d, "factor")
  expect_equal(as.character(result$swtest.d), "rejected")
  gated <- summary.impl(value ~ 1, d, stat.n = TRUE, stat.ad.test = 1, stat.sw.test = 1)
  expect_named(gated, c("dv.name", "n"))
  expect_named(summary.impl(value ~ 1, d, stat.sw.test = 1), "dv.name")
  d$group <- "a"
  expect_named(summary.impl(value ~ group, d, stat.sw.test = 1), "group")
  small <- summary.impl(value ~ 1, d[1:10, , drop = FALSE], stat.n = TRUE,
                        stat.skew.test = 1, stat.kurt.test = 1, stat.dago.test = 1)
  expect_named(small, c("dv.name", "n"))
})

test_that("vector and data frame inputs and all-variable wrappers retain statistic names", {
  expect_equal(summary.impl(c(1, 2, NA, 6), stat.mean = TRUE)$mean, 3)
  d <- data.frame(first = c(1, 3, NA, 5), second = c(2, 4, 6, 8),
                  category = factor(c("a", "a", "b", NA)))
  result <- summary.impl(d, stat.n = TRUE)
  expect_named(result, c("dv.name", "n"))
  expect_equal(result$dv.name, names(d))
  expect_equal(result$n, c(3, 4, 3))
  means <- summary.all.variables(d, summary.default = summary.impl, stat.mean = TRUE)
  expect_named(means, c("dv.name", "mean"))
  expect_equal(means$dv.name, names(d))
  expect_equal(means$mean, c(3, 5, NA))
  default <- summary.all.variables(d)
  expect_equal(default$dv.name, names(d))
  expect_equal(default$n, c(3, 4, 3))
  expect_equal(default$missing, c(1, 0, 1))
})

test_that("summary restores the warning option after success and errors", {
  previous <- getOption("warn")
  on.exit(options(warn = previous))
  options(warn = 1)
  d <- data.frame(value = 1:4, group = "a")
  summary.impl(value ~ group, d, stat.mean = TRUE)
  expect_identical(getOption("warn"), 1L)
  expect_error(summary.impl(absent ~ group, d, stat.mean = TRUE))
  expect_identical(getOption("warn"), 1L)
})
