## Create Ecodata Indicator Functions

#' Create Cold Pool Index
#'
#' This function generates a cold pool index indicator from ecodata R package to an R object
#' Cold pool index: quantifies the interannual strength of the cold pool
#' Positive values == colder 
#' Negative values == warmer
#' 
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_index`, returns cold pool index indicator
#' @importFrom magrittr %>%
#' @export
 

create_coldpool_index <- function(data){
  coldpool_index <- data |>
    dplyr::filter(Var == 'cold_pool_index') %>%
    dplyr::rename(YEAR = Time,
                  DATA_VALUE = Value,
                  INDICATOR_NAME = Var) %>%
    subset(select = -c(source))
  
  return(coldpool_index)
}

#' Create Cold Pool Extent
#'
#' This function generates a cold pool extent indicator from ecodata R package to an R object
#' Cold pool extent:  total area where bottom temperatures remain below 10 °C for at least 2 months between June and September
#' Positive values == Larger
#' Negative values == Smaller 
#' 
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_extent`, returns cold pool extent indicator
#' @importFrom magrittr %>%
#' @export


create_coldpool_extent <- function(data){
  coldpool_extent <- data |>
    dplyr::filter(Var == 'extent_index') %>%
    dplyr::rename(YEAR = Time,
                  DATA_VALUE = Value,
                  INDICATOR_NAME = Var) %>%
    subset(select = -c(source))
  
  return(coldpool_extent)
}


#' Create Cold Pool Persistence
#'
#' This function generates a cold pool persistence indicator from ecodata R package to an R object
#' Cold pool persistence: duration of the cold pool, which ends when bottom temperature rises above 10 °C after it is formed each year
#' Positive values == Longer
#' Negative values == Shorter
#'
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_persistence`, returns cold pool persistence indicator
#' @importFrom magrittr %>%
#' @export

create_coldpool_persistence <- function(data){
  coldpool_persistence <- data |>
    dplyr::filter(Var == 'persistence_index') %>%
    dplyr::rename(YEAR = Time,
                  DATA_VALUE = Value,
                  INDICATOR_NAME = Var) %>%
    subset(select = -c(source))
  
  return(coldpool_persistence)
}


#' Create Gulf stream Index
#'
#' This function generates a Gulf Stream index indicator from ecodata R package to an R object
#' Gulf Stream Index: measure of the Gulf Stream position relative to the mean position
#' Positive values == More northerly
#' Negative values == More southerly
#' 
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `gsi`, returns Gulf Stream Index indicator
#' @importFrom magrittr %>%
#' @export

create_gsi <- function(data){
  gsi <- data |>
    tidyr::separate(Time, c("Year", "Month"), sep = "\\.") %>%
    dplyr::rename(YEAR = Year,
                  MONTH = Month,
                  INDICATOR_NAME = Var,
                  DATA_VALUE = Value)
  
  return(gsi)
}


#' Create Warm Core Rings
#'
#' This function generates a warm core rings indicator from ecodata R package to an R object
#' Warm Core Rings: number of warm core ring formations
#' 
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `wcr`, returns warm core rings indicator
#' @importFrom magrittr %>%
#' @export

create_wcr <- function(data){
  wcr <- data |>
    dplyr::rename(YEAR = Time,
                  INDICATOR_NAME = Var,
                  DATA_VALUE = Value,
                  INDICATOR_UNITS = Units)
  
  return(wcr)
}

