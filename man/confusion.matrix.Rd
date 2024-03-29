% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion.matrix.R
\name{confusion.matrix}
\alias{confusion.matrix}
\title{Create a confusion matrix}
\usage{
confusion.matrix(
  actual,
  predicted,
  actual.positive.fn = function(actual) {
     ifelse(actual == 1, 1, 0)
 },
  predicted.positive.fn = function(predicted) {
     ifelse(predicted == 1, 1, 0)
 }
)
}
\arguments{
\item{actual}{A vector of values identifying the actual value (default 0,1 or use actual.positive.fn to convert)}

\item{predicted}{A vector of values identifying the predicted value (default 0,1 or use predicted.positive.fn to convert)}

\item{actual.positive.fn}{A function that takes values present in actual and converts them to 0,1}

\item{predicted.positive.fn}{A function that takes values in predicted and converts them to 0,1}
}
\value{
A 2x2 matrix showing agreement between actual and predicted.
}
\description{
Create a confusion matrix from actual and predicted values. Note that this function follows the convention 
of placing "Predicted" as the row variable and "Actual/Correct" as the column variable for the returned contingency 
table. This is unlikely to be a compatible format with other packages that use the reverse convention for predicted/actual.
}
