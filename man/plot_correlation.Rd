% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression_functions.R
\name{plot_correlation}
\alias{plot_correlation}
\title{Plots correlations between stock data and indicators}
\usage{
plot_correlation(data, lag = 0)
}
\arguments{
\item{data}{The output of \code{data_prep()}.}

\item{lag}{The number of years to lag the correlation by. Defaults to 0.}
}
\value{
A ggplot
}
\description{
This function plots correlations between stock data and \code{ecodata} data.
}
