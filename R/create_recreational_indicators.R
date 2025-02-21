#' Create MRIP total recreational catch indicator
#'
#' This function creates the total recreational catch indicator
#' @param data The mrip data (R object `mrip_catch`), already subset to species of interest only from downloaded csv file
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `total_rec_catch`
#'
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Total Catch
#' Download csv as output
#'
#' @export
#'
`%>%` <- magrittr::`%>%`

create_total_rec_catch <- function(data, 
                                   states = c('MAINE',
                                              'CONNECTICUT',
                                              'MASSACHUSETTS',
                                              'NEW HAMPSHIRE',
                                              'NEW JERSEY',
                                              'NEW YORK',
                                              'RHODE ISLAND',
                                              'MARYLAND',
                                              'DELAWARE',
                                              'NORTH CAROLINA'),
                                   return = TRUE){
  total_rec_catch <- data %>%
    dplyr::rename(tot_cat = Total.Catch..A.B1.B2.) %>%
    dplyr::filter(State %in% states) %>%
    dplyr::group_by(Year, State) %>% # remove State if wanting all states summed
    dplyr::summarise(DATA_VALUE = sum(tot_cat, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_catch_n") %>%
    dplyr::rename(YEAR = Year,
                  STATE = State) %>% #remove STATE = State if wanting all states summed
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  if(return) return(total_rec_catch)
}


#' Create MRIP total recreational trips indicator
#'
#' This function creates a total recreational trips indicator
#' @param files A list of the full file names of annual directed trip data (.csv format). Must download for each year in MRIP query tool. 
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `rec_trips`, returns directed recreational trips indicator
#'
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Directed Trip' under 'Effort Data'.
#' Choose year of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Primary Target
#' Download csv as output
#'
#'
#' @export
`%>%` <- magrittr::`%>%`

create_rec_trips <- function(files, 
                             states = c('MAINE',
                                        'CONNECTICUT',
                                        'MASSACHUSETTS',
                                        'NEW HAMPSHIRE',
                                        'NEW JERSEY',
                                        'NEW YORK',
                                        'RHODE ISLAND',
                                        'MARYLAND',
                                        'DELAWARE',
                                        'NORTH CAROLINA'),
                             return = TRUE) {
  rec_directed_trips <- c()
  for (i in files) {
    this_dat <- read.csv(i,
                         skip = 44,# was 24 is now 44
                         na.strings = "."
    )
    message(unique(this_dat$Year)) 
    rec_directed_trips <- rbind(rec_directed_trips, this_dat)
  }
  
  rec_trips <- rec_directed_trips %>%
    dplyr::filter(State %in% states) %>%
    dplyr::group_by(Year, State) %>% # remove State want all states summed
    dplyr::summarise(DATA_VALUE = sum(Directed.Trips, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "rec_trips") %>%
    dplyr::rename(YEAR = Year,
                  STATE = State) # remove STATE = State if want all states summed
  
  if(return) return(rec_trips)
  
}

#' Create MRIP species recreational effort indicator
#'
#' This function creates an indicator with the proportion trips targeting a species of interest
#' @param total_trips The MRIP trip data (R object `mrip_effort`) of total annual angler trips from downloaded csv file
#' @param species_trips The directed trips data for species of interest (R object `rec_trips`) from function 'create_rec_trips'
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `prop_sp_trips`
#'
#'
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Effort Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, all modes and areas
#' Download csv as output
#'
#' @export
`%>%` <- magrittr::`%>%`

create_prop_sp_trips <- function(total_trips,
                                 species_trips,
                                 states = c('MAINE',
                                            'CONNECTICUT',
                                            'MASSACHUSETTS',
                                            'NEW HAMPSHIRE',
                                            'NEW JERSEY',
                                            'NEW YORK',
                                            'RHODE ISLAND',
                                            'MARYLAND',
                                            'DELAWARE',
                                            'NORTH CAROLINA'),
                                 return = TRUE){
  total_trips <- total_trips %>%
    dplyr::filter(State %in% states) %>% 
    dplyr::group_by(Year, State) %>% # remove State here if want all states summed
    dplyr::summarise(total_trips = sum(as.numeric(Angler.Trips), na.rm = TRUE)) %>% 
    dplyr::mutate(YEAR = as.numeric(Year)) %>%
    dplyr::select(-Year) 
  sp <- species_trips %>% 
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(DATA_VALUE = sum(as.numeric(DATA_VALUE), na.rm = TRUE))
  prop_sp_trips <- dplyr::full_join(total_trips,
                                    sp,
                                    by = c("YEAR")) %>%
    dplyr::mutate(DATA_VALUE = DATA_VALUE/total_trips,
                  INDICATOR_NAME = "proportion_sp_trips") %>%
    dplyr::select(-total_trips) 
  # comment out the following lines if wanting all states summed
  dplyr::ungroup() %>%
    dplyr::select(-Year) 
  dplyr::rename(STATE = State)
  
  if(return) return(prop_sp_trips)
}


#' Create MRIP recreational landings indicator
#'
#' This function creates an indicator for total recreational landings
#' @param data The MRIP harvest data (R object `mrip_landing`)
#' #' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `total_rec_landings`
#'
#'
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, species of interest, all modes and areas, Harvest (A + B1), choose # of fish/weight (lbs), mean length and weight
#' Download csv as output
#'
#'
#' @export
`%>%` <- magrittr::`%>%`

create_total_rec_landings <- function(data, 
                                      states = c('MAINE',
                                                 'CONNECTICUT',
                                                 'MASSACHUSETTS',
                                                 'NEW HAMPSHIRE',
                                                 'NEW JERSEY',
                                                 'NEW YORK',
                                                 'RHODE ISLAND',
                                                 'MARYLAND',
                                                 'DELAWARE',
                                                 'NORTH CAROLINA'),
                                      return = TRUE){
  total_rec_landings <- data %>%
    dplyr::rename(lbs_ab1 = Harvest..A.B1..Total.Weight..lb.) %>%
    dplyr::filter(State %in% states) %>%
    dplyr::group_by(Year, State) %>% #remove State here if want all states summed
    dplyr::summarise(DATA_VALUE = sum(lbs_ab1, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_landings_lbs") %>%
    dplyr::rename(YEAR = Year,
                  STATE = State) %>% #remove STATE = State here if want all states summed
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  if(return) return(total_rec_landings)
}

