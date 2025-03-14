% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_survdat_functions.R
\name{get_len_data2}
\alias{get_len_data2}
\title{Format \code{survdat} length data for data table}
\usage{
get_len_data2(x)
}
\arguments{
\item{x}{A \code{survdat} data frame or tibble, containing data on one species.}
}
\value{
A tibble
}
\description{
This function formats \code{survdat} length data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year. Returns lengths in a wide format.
}
