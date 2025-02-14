### REC INDICATORS ###
### TOTAL CATCH, EFFORT, DIRECTED TRIPS, TOTAL LANDINGS ###

#' Create MRIP total recreational catch indicator
#'
#' This function creates the total recreational catch indicator
#' @param data The mrip data (R object `mrip_catch`), already subset to species of interest only from downloaded csv file
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `total_rec_catch`
#'
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, geographical area and species of interest, all modes and areas, Total Catch
#' Download csv as output
#'
#' @export
#'
`%>%` <- magrittr::`%>%`

#removes extra info at the top of file and formats  correct columns
mrip_catch <- read.csv(here::here('data-raw/mrip_BLACK_SEA_BASS_catch_series.csv'),
                       skip = 46, # of rows you want to ignore
                       na.strings = ".")

create_total_rec_catch <- function(data = mrip_catch, return = TRUE){
  total_rec_catch <- data %>%
    dplyr::rename(tot_cat = Total.Catch..A.B1.B2.) %>%
    dplyr::filter(State %in%
                    c('MAINE',
                      'CONNECTICUT',
                      'MASSACHUSETTS',
                      'NEW HAMPSHIRE',
                      'NEW JERSEY',
                      'NEW YORK',
                      'RHODE ISLAND',
                      'MARYLAND',
                      'DELAWARE',
                      'NORTH CAROLINA'
                    )) %>%
    dplyr::group_by(Year) %>% # changed y to Y
    dplyr::summarise(DATA_VALUE = sum(tot_cat, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_catch_n") %>%
    dplyr::rename(YEAR = Year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  # usethis::use_data(total_rec_catch, overwrite = TRUE)
  if(return) return(total_rec_catch)
}
 create_total_rec_catch(mrip_catch)
 