% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_recreational_indicators.R
\name{create_rec_trips}
\alias{create_rec_trips}
\title{Create MRIP total recreational trips indicator}
\usage{
create_rec_trips(
  files,
  states = c("MAINE", "CONNECTICUT", "MASSACHUSETTS", "NEW HAMPSHIRE", "NEW JERSEY",
    "NEW YORK", "RHODE ISLAND", "MARYLAND", "DELAWARE", "NORTH CAROLINA"),
  groupby_state = FALSE,
  return = TRUE
)
}
\arguments{
\item{files}{A list of the full file names of annual directed trip data (.csv format). Must download for each year in MRIP query tool.}

\item{states}{States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'}
}
\value{
Saves R object \code{rec_trips}, returns directed recreational trips indicator
}
\description{
This function creates a total recreational trips indicator
For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
Query 'Directed Trip' under 'Effort Data'.
Choose year of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Primary Target
Download csv as output
}
