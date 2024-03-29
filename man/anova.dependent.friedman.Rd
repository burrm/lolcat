% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova.dependent.friedman.R
\name{anova.dependent.friedman}
\alias{anova.dependent.friedman}
\title{Friedman Two-Way Analysis of Variance By Ranks}
\usage{
\method{anova}{dependent.friedman}(
  fx,
  data = NULL,
  order.by = 1:nrow(data),
  tie.correct = T,
  alternative = c("two.sided", "greater", "less"),
  conf.level = 0.95
)
}
\arguments{
\item{fx}{A formula defining groups and a dependent variable}

\item{data}{A data frame that corresponds to the formulas in fx.}

\item{order.by}{ordering vector for the data, does not need to be unique}

\item{tie.correct}{Tie correction (T/F)}

\item{alternative}{Alternative hypothesis to be tested}

\item{conf.level}{Confidence level for test}
}
\value{
htest object containing results of the test.
}
\description{
Perform Friedman's Analysis of Variance by Ranks.
}
