# channel <- dbutils::connect_to_database("server name", "user name")
data <- survdat::get_survdat_data(channel, getBio = T, getLengths = T)
species <- survdat::get_species(channel)

`%>%` <- magrittr::`%>%`

## create a subset of data to save in package for running vignette ----
survdat_subset <- data$survdat |>
  dplyr::filter(SVSPP == '141')
usethis::use_data(survdat_subset, overwrite = T)
usethis::use_data(species)
# this will create a data object in the data folder
# documentation for the data needs to be added manually
# everything in the data folder is loaded when you load the NEesp2 R package
roxygen2::roxygenise()


str(survdat_subset)
##species condition - BLACK SEA BASS## ----

condition <- species_condition (data = survdat_subset, LWparams = LWparams, species.codes = species.codes) 
bsb_condition <- condition %>%
  subset(Species == 'Black sea bass')
head(bsb_condition)

plot_condition(data = condition, var = 'Black sea bass')

##species range - Black sea bass in Fall##

range <- species_range(data = survdat_subset, species = species)
bsb_range <- range %>%
  subset(species == 'BLACK SEA BASS') %>%
  subset(SEASON == 'FALL') %>%
  subset(indicator_name == c('range_lat','range_lon')) %>%
  dplyr::rename(INDICATOR_NAME = indicator_name,
                DATA_VALUE = indicator_value) 
head(bsb_range)

bsb_range %>%
  ggplot2::ggplot(ggplot2::aes(x = YEAR,
                               y = DATA_VALUE,
                               color = INDICATOR_NAME,
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_path() + 
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Black Sea Bass Range in Fall (Lat + Lon)", 
                x = "Year",
                y = "Range (degrees)") +
  ggplot2::facet_wrap('INDICATOR_NAME')

##diet##

allfh <- get(load(here::here("data-raw/allfh.RData")))
diet <- diet(data = allfh)
head(diet)

######################################################################

##swept area biomass - black sea bass in fall##

bsb_swept_area <- create_swept_area(
  surveyData = survdat_subset,
  areaPolygon = "NEFSC strata",
  areaDescription = "STRATA",
  filterByArea = "all",
  filterBySeason = "FALL",
  groupDescription = "SVSPP",
  filterByGroup = "all",
  mergesexFlag = T,
  tidy = F,
  q = NULL,
  a = 0.0384
) %>%
  dplyr::filter(SVSPP == "141")

bsb_swept_area %>%
  ggplot2::ggplot(ggplot2::aes(x = Year,
                               y = indicator_value,
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_path() + 
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Black Sea Bass Swept Area Biomass (Fall)", 
                x = "Year",
                y = "Swept Area Biomass") 

##stratified mean biomass in fall##

bsb_strat_mean <- create_stratified_mean(
  surveyData = survdat_subset,
  areaPolygon = "NEFSC strata",
  areaDescription = "STRATA",
  filterByArea = "all",
  filterBySeason = "FALL",
  groupDescription = "SVSPP",
  filterByGroup = "all",
  mergesexFlag = T,
  tidy = F,
  returnPrepData = F
) %>%
  dplyr::filter(SVSPP == "141")

bsb_strat_mean %>%
  ggplot2::ggplot(ggplot2::aes(x = Year,
                               y = indicator_value,
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_path() + 
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Black Sea Bass Stratified Mean Biomass (Fall)", 
                x = "Year",
                y = "Stratified Mean Biomass") 

