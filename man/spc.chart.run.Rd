% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spc.chart.run.R
\name{spc.chart.run}
\alias{spc.chart.run}
\alias{spc.run.chart}
\title{SPC Run Chart}
\usage{
spc.chart.run(
  chart.series,
  type = "b",
  main = "Run Chart",
  xlab = "Time",
  ylab = "Criterion Measure",
  col = "blue",
  pch = 19,
  ...
)

spc.run.chart(
  chart.series,
  type = "b",
  main = "Run Chart",
  xlab = "Time",
  ylab = "Criterion Measure",
  col = "blue",
  pch = 19,
  ...
)
}
\arguments{
\item{chart.series}{Vector - Chart series}

\item{type}{Character - Chart display type, default is line and point}

\item{main}{Character - Main chart title}

\item{xlab}{Character - X axis title}

\item{ylab}{Character - Y axis title}

\item{col}{Character - Color of lines/points}

\item{pch}{Integer - Point Display Character}

\item{...}{Additional parameters}
}
\value{
Invisible return of plot(...).
}
\description{
Provides a simple control chart a series without calculating limits
}
