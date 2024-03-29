% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi.square.independence.test.simple.R
\name{chi.square.independence.test.simple}
\alias{chi.square.independence.test.simple}
\title{Chi Square Independence Test}
\usage{
chi.square.independence.test.simple(
  observed.frequencies,
  expected.frequencies = chi.square.2d.expected.frequencies(observed.frequencies),
  alternative = c("greater", "less", "two.sided"),
  conf.level = 0.95
)
}
\arguments{
\item{observed.frequencies}{A matrix of values to test.}

\item{expected.frequencies}{A matrix of values to compare with observed.frequencies.}

\item{alternative}{The alternative hypothesis to use for the test computation.}

\item{conf.level}{The confidence level for this test, between 0 and 1.}
}
\value{
The results of the statistical test.
}
\description{
Calculates chi square independence test for given contingency table.
}
