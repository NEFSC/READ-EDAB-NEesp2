
#' Utility function to group by
#' 
#' This function groups by year or by year and state
#' 
#' @param data the data (a tibble)
#' @param groupby Boolean, if TRUE will group by STATE and YEAR, if false will group by YEAR only

groupby_state  <- function(data, groupby) {
  if(groupby) {
    data <- data |>
      dplyr::group_by(.data$YEAR, .data$STATE)} 
  else {
    data <- data |>
      dplyr::group_by(.data$YEAR)
  }
  
  return(data)
}



#' Create MRIP total recreational catch indicator
#'
#' This function creates the total recreational catch indicator
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Total Catch
#' Download csv as output
#' 
#' @param data The mrip data (R object `mrip_catch`), already subset to species of interest only from downloaded csv file
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @param groupby_state Whether to group results by state
#' @importFrom magrittr %>%
#' @return Saves the R data object `total_rec_catch`
#' @export

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
                                   groupby_state = FALSE,
                                   return = TRUE){
  total_rec_catch <- data %>%
    dplyr::rename(tot_cat = TOTAL_CATCH_A_B1_B2) %>%
    dplyr::filter(STATE %in% states) |>
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(tot_cat, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_catch_n") %>%
    # dplyr::rename(YEAR = Year,
    #               STATE = State) %>% #remove STATE = State if wanting all states summed
    dplyr::ungroup()## %>%
    # dplyr::mutate(YEAR = as.numeric(YEAR))
  
  if(return) return(total_rec_catch)
}


#' Create MRIP total recreational trips indicator
#'
#' This function creates a total recreational trips indicator
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Directed Trip' under 'Effort Data'.
#' Choose year of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Primary Target
#' Download csv as output
#' @param files A list of the full file names of annual directed trip data (.csv format). Must download for each year in MRIP query tool. 
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @return Saves R object `rec_trips`, returns directed recreational trips indicator
#' @export
# `%>%` <- magrittr::`%>%`

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
                             groupby_state = FALSE,
                             return = TRUE) {
  rec_directed_trips <- c()
  for (i in files) {
    this_dat <- read.csv(i,
                         skip = 44,# was 24 is now 44
                         na.strings = "."
    )
    # message(unique(this_dat$Year)) 
    rec_directed_trips <- rbind(rec_directed_trips, this_dat)
  }
  
  rec_trips <- rec_directed_trips %>%
    janitor::clean_names(case = "all_caps") %>%
    dplyr::filter(STATE %in% states) %>%
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(DIRECTED_TRIPS, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "rec_trips")
  
  if(return) return(rec_trips)
  
}

#' Create MRIP species recreational effort indicator
#'
#' This function creates an indicator with the proportion trips targeting a species of interest
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Effort Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, all modes and areas
#' Download csv as output
#'
#' @param total_trips The MRIP trip data (R object `mrip_effort`) of total annual angler trips from downloaded csv file
#' @param species_trips The directed trips data for species of interest (R object `rec_trips`) from function 'create_rec_trips'
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @return Saves the R data object `prop_sp_trips`
#' @export
# `%>%` <- magrittr::`%>%`

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
                                 groupby_state = FALSE,
                                 return = TRUE){
  total_trips <- total_trips %>%
    dplyr::filter(STATE %in% states) %>% 
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(total_trips = sum(as.numeric(ANGLER_TRIPS), na.rm = TRUE)) %>% 
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  sp <- species_trips %>% 
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(as.numeric(DATA_VALUE), na.rm = TRUE))
  
  prop_sp_trips <- dplyr::full_join(total_trips,
                                    sp,
                                    by = c("YEAR")) %>%
    dplyr::mutate(DATA_VALUE = DATA_VALUE/total_trips,
                  INDICATOR_NAME = "proportion_sp_trips") %>%
    dplyr::select(-total_trips) |>
  # comment out the following lines if wanting all states summed
  dplyr::ungroup() #%>%
    # dplyr::select(-Year) 
  # dplyr::rename(STATE = State)
  
  if(return) return(prop_sp_trips)
}


#' Create MRIP recreational landings indicator
#'
#' This function creates an indicator for total recreational landings
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, species of interest, all modes and areas, Harvest (A + B1), choose # of fish/weight (lbs), mean length and weight
#' Download csv as output
#' 
#' @param data The MRIP harvest data (R object `mrip_landing`)
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @return Saves the R data object `total_rec_landings`
#' @export
# `%>%` <- magrittr::`%>%`

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
                                      groupby_state = FALSE,
                                      return = TRUE){
  total_rec_landings <- data %>%
    dplyr::rename(lbs_ab1 = HARVEST_A_B1_TOTAL_WEIGHT_LB) %>%
    dplyr::filter(STATE %in% states) %>%
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(lbs_ab1, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_landings_lbs") %>%
    # dplyr::rename(YEAR = Year,
    #               STATE = State) %>% #remove STATE = State here if want all states summed
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  if(return) return(total_rec_landings)
}

