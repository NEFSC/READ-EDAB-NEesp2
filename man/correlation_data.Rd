% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression_functions.R
\name{correlation_data}
\alias{correlation_data}
\title{Produces summary tables of correlations}
\usage{
correlation_data(data, lag = 0)
}
\arguments{
\item{data}{The output of \code{data_prep()}.}

\item{lag}{The number of years to lag the correlation by. Defaults to 0.}
}
\value{
A data frame
}
\description{
This function creates summary tables of correlations between stock data and \code{ecodata} data. Designed for use within a RMarkdown document.
}
