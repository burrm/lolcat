% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msa.nominal.internalconsistency.R
\name{msa.nominal.internalconsistency}
\alias{msa.nominal.internalconsistency}
\title{Perform Study of Nominal Scale Rater's Internal Consistency}
\usage{
msa.nominal.internalconsistency(..., conf.level = 0.95)
}
\arguments{
\item{...}{Vectors - character or factor - One or more rater's evaluations of subjects}

\item{conf.level}{Confidence level to use for the statistical tests}
}
\value{
A data structure including details of the tests between measurements. Use summary() to summarize results.
}
\description{
Evaluate two or more measurements' agreement for a set of subjects, usually within a single rater.
}
