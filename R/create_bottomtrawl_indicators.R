
#' Calculate Swept Area Biomass
#' 
#' This function calculates swept area biomass
#' 
#' @param ... passed to `survdat::calc_swept_area()`. Parameters here: https://noaa-edab.github.io/survdat/reference/calc_swept_area.html
#' @importFrom magrittr %>%
#' @return a data table 'swept_area'


# to use EPU shapefile run code below prior to function and set parameters areaPolygon=area and areaDescription="EPU":
#area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) 
create_swept_area <- function(...) { 
  swept_area <- survdat::calc_swept_area(...) %>%
    dplyr::select(YEAR, SVSPP, tot.biomass, tot.bio.var, tot.bio.SE) %>%
    dplyr::rename(swept_area_biomass = tot.biomass, 
                  variance = tot.bio.var,
                  se = tot.bio.SE) %>%
    dplyr::mutate(sd = sd(swept_area_biomass, na.rm = TRUE),
                  INDICATOR_UNITS = "numberstow-1") %>%
    tidyr::pivot_longer(cols = c("swept_area_biomass")) %>%
    dplyr::rename(INDICATOR_NAME = name,
                  DATA_VALUE = value)
  
  return(swept_area)
}


#' Calculate Stratified Mean Biomass
#' 
#' This function calculates stratified mean biomass
#' 
#' @param ... passed to `survdat::calc_stratified_mean()`. Parameters here: https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.html
#' @importFrom magrittr %>%
#' @return a data table 'strat_mean'

# to use EPU shapefile run code below prior to function and set parameters areaPolygon=area and areaDescription="EPU":
#area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T) 
create_stratified_mean <- function(...) { 
  strat_mean <- survdat::calc_stratified_mean(...) %>%
    dplyr::select(YEAR, SEASON, SVSPP, strat.biomass, biomass.var, biomass.SE) %>%
    dplyr::rename(strat_mean_biomass = strat.biomass, 
                  variance = biomass.var, 
                  se = biomass.SE) %>%
    dplyr::mutate(sd = sd(strat_mean_biomass, na.rm = TRUE),
                  INDICATOR_UNITS = "kgtow-1") %>%
    tidyr::pivot_longer(cols = c("strat_mean_biomass")) %>%
    dplyr::rename(INDICATOR_NAME = name,
                  DATA_VALUE = value)
  
  
  return(strat_mean)
}


#' Create a table of diet data
#'
#' @param data A data frame from `allfh`.
#' @return A `DT::datatable` or a `knitr::kable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

 #data <- get(load(here::here("data-raw/allfh.RData")))

diet <- function(data) {
  if (nrow(data) > 0) {
    normalized <- data %>%
      dplyr::filter(.data$pyamtw > 0) %>%
      
      # only look at season/year combinations with >20 predator samples
      dplyr::group_by(.data$year, .data$season, .data$geoarea) %>%
      dplyr::mutate(n_predators = .data$pdcomnam %>%
                      unique() %>%
                      length()) %>%
      dplyr::filter(.data$n_predators > 20)
    
    if (length(normalized$n_predators) > 1) {
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(total_weight = sum(.data$pyamtw)) %>%
        dplyr::mutate(proportion = .data$total_weight / sum(.data$total_weight))
      
      normalized$gensci <- stringr::str_replace(normalized$gensci, " ", "_")
      
      # group low abundance prey as "other"
      groups <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(max_prop = max(.data$proportion)) %>%
        dplyr::filter(.data$max_prop > 0.05)
      
      groups <- groups$gensci %>%
        unique()
      
      rows <- match(normalized$gensci, groups) %>%
        is.na() %>%
        which()
      
      normalized[rows, "gensci"] <- "OTHER"
      
      # re-group proportions with new "other" category
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(prop2 = sum(.data$proportion))
      
      # summary table
      table <- normalized %>%
        dplyr::group_by(.data$gensci, .data$season, .data$geoarea, .data$year) %>%
        dplyr::filter(sum(.data$prop2) > 0) %>%
        dplyr::group_by(.data$gensci, .data$season, .data$geoarea, .data$year) %>%
        dplyr::rename(prey_name = 'gensci', region = 'geoarea') %>%
        dplyr::summarise(
          mean_proportion = paste(mean(.data$prop2) %>% round(digits = 3),
                                  " +- ",
                                  stats::sd(.data$prop2) %>% round(digits = 3),
                                  " (", length(.data$prop2), ") ",
                                  sep = ""
          ),
          
          range_proportion = paste(min(.data$prop2) %>% round(digits = 3),
                                   max(.data$prop2) %>% round(digits = 3),
                                   sep = " - "
          )
        )
    } else {
      print("NOT ENOUGH DATA")
    }
  } else {
    print("NO DATA")
  }
}

#' Calculate Species Range
#'
#' @param data data table. NEFSC survey data generated by 'survdat::get_survdat_data(channel)'
#' @param species data table. NEFSC species data generated by 'survdat::get_species(channel)'
#' @return a data frame
#' @importFrom magrittr %>%
#' @export

species_range <- function(data, species) {
  
  
  #filter surveyData to year, season, lat, lon, and species code  
  survdat_data <- data$survdat %>%
    dplyr::select(SVSPP, YEAR, SEASON, LAT, LON)
  
  #get species names, filter to species code, scientific + common name
  species_data <- species$data %>%
    tidyr::drop_na(SVSPP) %>%
    dplyr::select (SVSPP, SCINAME, COMNAME) 
  
  #join survdat df with species df grouping by species code
  join <- dplyr::full_join(survdat_data, species_data %>%
                             dplyr::group_by(SVSPP)) 
  
  ##min/max lat/lon for all tows in fall and spring for each year
  all_tows <- join %>%
    dplyr::group_by(YEAR, SEASON) %>%
    dplyr::summarise(min_lat = min(LAT),
                     max_lat = max(LAT),
                     min_lon = min(LON),
                     max_lon = max(LON)) %>%
    tidyr::pivot_longer(cols = c("min_lat", "max_lat", "min_lon", "max_lon")) %>% 
    dplyr::rename(indicator_name = name,
                  indicator_value = value) %>%
    dplyr::ungroup() 
  
  all_tows$species <- "ALL" 
  all_tows$SVSPP <- 0
  
  
  ##min/max/range grouped by species and species code
  all_species <- join %>%
    dplyr::group_by(YEAR, SEASON, SVSPP, COMNAME) %>%
    dplyr::summarise(min_lat = min(LAT),
                     max_lat = max(LAT),
                     min_lon = min(LON),
                     max_lon = max(LON),
    ) %>%
    dplyr::mutate(range_lat = max_lat - min_lat,
                  range_lon = max_lon - min_lon) %>%
    tidyr::pivot_longer(cols = c("min_lat", "max_lat", "min_lon", "max_lon", "range_lat", "range_lon")) %>%
    dplyr::rename(indicator_name = name,
                  indicator_value = value,
                  species = COMNAME) %>%
    dplyr::ungroup()
  
  ##join all_tows and all_species
  range <- rbind(all_tows, all_species) %>%
    dplyr::rename(INDICATOR_NAME = indicator_name,
                  DATA_VALUE = indicator_value,
                  SPECIES = species) %>%
    dplyr::mutate(INDICATOR_UNITS = "degrees")
  return(range)
} 
  