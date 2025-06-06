% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mrip_data.R
\name{save_trips}
\alias{save_trips}
\title{Scrape and save MRIP trip data from MRIP Query tool}
\usage{
save_trips(this_species, this_year, this_region, out_folder)
}
\arguments{
\item{this_species}{the common name of the species as it appears in the MRIP data. capitalization does not matter.}

\item{this_year}{the year of data to query. Must be a single value. The earliest year possible is 1981.}

\item{this_region}{the name of the region. Can be a state name, "North Atlantic", "Mid-Atlantic", etc. Capitalization does not matter.}

\item{out_folder}{where to save the data}
}
\value{
Returns a list of the scraped data and metadata.
}
\description{
This function scrapes MRIP trip data from the MRIP Query tool and saves it as an Rds.
Used as a helper function to automate data pulls.
}
