% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframeModifiers.R
\name{moveCol}
\alias{moveCol}
\title{Moves a data.frame column to a new position}
\usage{
moveCol(df, from = "last", to = 1)
}
\arguments{
\item{df}{An R data.frame}

\item{from}{an integer indicating the current position of the column to move.}

\item{to}{an integer indicating the target position of the column to move.}
}
\value{
A data.frame identical to the input data.frame in terms of contents but with reordered columns.
}
\description{
Moves a data.frame column to a new position
}
\details{
This function does not work with data tables. The default position for 'from' is the last column of the data.frame.
}

