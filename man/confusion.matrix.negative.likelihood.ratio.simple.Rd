% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion.matrix.negative.likelihood.ratio.R,
%   R/confusion.matrix.negative.likelihood.ratio.simple.R
\name{confusion.matrix.negative.likelihood.ratio}
\alias{confusion.matrix.negative.likelihood.ratio}
\alias{confusion.matrix.negative.likelihood.ratio.simple}
\title{Confusion Matrix Measures - Negative Likelihood Ratio}
\usage{
confusion.matrix.negative.likelihood.ratio(confusion.matrix)

confusion.matrix.negative.likelihood.ratio.simple(
  false.negative = 0,
  true.positive = 1,
  count.positive = false.negative + true.positive,
  true.negative = 0,
  false.positive = 1,
  count.negative = true.negative + false.positive,
  true.negative.rate = confusion.matrix.true.negative.rate.simple(true.negative =
    true.negative, count.negative = count.negative),
  false.negative.rate = confusion.matrix.false.negative.rate.simple(false.negative =
    false.negative, count.positive = count.positive)
)
}
\arguments{
\item{confusion.matrix}{Matrix - confusion matrix.}

\item{false.negative}{Scalar - Cases identified as false negative - optional if FNR specified}

\item{true.positive}{Scalar - Cases identified as true positive - optional if  FNR specified}

\item{count.positive}{Scalar - Total cases identified as positive - optional if TNR specified.}

\item{true.negative}{Scalar - Cases identified as true negative - optional if TNR specified}

\item{false.positive}{Scalar - Cases identified as false positive - optional if TNR specified}

\item{count.negative}{Scalar - Total cases identified as negative - optional if FNR specified.}

\item{true.negative.rate}{Scalar - True Negative Rate (TNR) - optional if TNR parameters specified.}

\item{false.negative.rate}{Scalar - False Negative Rate (FNR) - optional if FNR parameters specified.}
}
\value{
A scalar with computed value.
}
\description{
Calculate Negative Likelihood Ratio, defined as FNR/TNR, for a given confusion matrix.
}
