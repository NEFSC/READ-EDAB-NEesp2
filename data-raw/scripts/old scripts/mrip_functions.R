#' Utility function to group by
#'
#' This function groups by year or by year and state
#'
#' @param data the data (a tibble)
#' @param groupby Boolean, if TRUE will group by STATE and YEAR, if false will group by YEAR only

groupby_state <- function(data, groupby) {
  if (groupby) {
    data <- data |>
      dplyr::group_by(.data$YEAR, .data$STATE)
  } else {
    data <- data |>
      dplyr::group_by(.data$YEAR)
  }
  
  return(data)
}


#' Read in MRIP recreational catch data
#'
#' This function read in MRIP catch data
#'
#' @param spceies the species common name
#' @param dir the directory where MRIP catch files are saved
#' @return a tibble
#' @export

## TODO: not sure this function is needed anymore

read_rec_catch <- function(species, dir, type = "all") {
  new_species <- species |>
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "_")
  
  files <- list.files(
    path = dir,
    full.names = TRUE
  )
  
  this_file <- files[which(stringr::str_detect(stringr::str_to_upper(files),
                                               pattern = paste0(
                                                 stringr::str_to_upper(type),
                                                 "_",
                                                 stringr::str_to_upper(species)
                                               ) |>
                                                 stringr::str_replace_all(" ", "_")
  ))]
  
  # read in the data
  rec_catch <- readRDS(this_file) |>
    purrr::map(~ janitor::clean_names(.x[1],
                                      case = "all_caps"
    ))
  
  return(rec_catch)
}

#'
#' This function returns a list of MRIP trip files
#' @param dir A directory that has subfolders with species-level data
#' @param species the species of interest
#' @return A vector of files
#' @export

get_trip_files <- function(dir, species) {
  new_dir <- list.dirs(dir,
                       full.names = TRUE
  )
  this_dir <- new_dir[which(stringr::str_detect(stringr::str_to_upper(new_dir),
                                                pattern = stringr::str_to_upper(species) |>
                                                  stringr::str_replace_all(" ", "_")
  ))]
  
  files <- list.files(this_dir,
                      pattern = "[0-9].Rds",
                      full.names = TRUE
  )
  
  return(files)
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

## TODO: I think this function can be deprecated

create_total_rec_landings <- function(data,
                                      states = c(
                                        "MAINE",
                                        "CONNECTICUT",
                                        "MASSACHUSETTS",
                                        "NEW HAMPSHIRE",
                                        "NEW JERSEY",
                                        "NEW YORK",
                                        "RHODE ISLAND",
                                        "MARYLAND",
                                        "DELAWARE",
                                        "NORTH CAROLINA"
                                      ),
                                      groupby_state = FALSE,
                                      return = TRUE) {
  total_rec_landings <- data %>%
    dplyr::rename(lbs_ab1 = HARVEST_A_B1_TOTAL_WEIGHT_LB) %>%
    dplyr::filter(STATE %in% states) %>%
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(lbs_ab1, na.rm = TRUE)) %>%
    dplyr::mutate(
      CATEGORY = "Recreational",
      INDICATOR_TYPE = "Socioeconomic",
      INDICATOR_NAME = "total_recreational_landings_lbs",
      INDICATOR_UNITS = "lbs"
    ) %>%
    # dplyr::rename(YEAR = Year,
    #               STATE = State) %>% #remove STATE = State here if want all states summed
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  if (return) {
    return(total_rec_landings)
  }
}
