% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_survdat_functions.R
\name{data_summary}
\alias{data_summary}
\title{Summarise \code{survdat} data}
\usage{
data_summary(x)
}
\arguments{
\item{x}{A data frame or tibble, containing data on one species. The output of \code{get_var_data}.}
}
\value{
A \code{DT::datatable}
}
\description{
This function summarises \code{survdat} data, grouping by by region. Data must be pre-processed with \code{get_var_data}.
}
