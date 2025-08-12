`%>%` <- magrittr::`%>%`

## Pull survdat data
# channel <- dbutils::connect_to_database("server name", "user name")
surveyData <- survdat::get_survdat_data(channel)

#filter survdat data to year, season, lat, and species code
survdat_data <- data$survdat %>%
  dplyr::select(SVSPP, YEAR, SEASON, LAT, LON)

#get species names, filter to species code, scientific + common name
species <- survdat::get_species(channel)
species_data <- species$data %>%
  tidyr::drop_na(SVSPP) %>%
  dplyr::select (SVSPP, SCINAME, COMNAME)

#join survdat df with species df grouping by species code
join <- dplyr::full_join(survdat_data, species_data %>%
                   dplyr::group_by(SVSPP))

#test subsetting for one species (acadian redfish) 
#redfish <- subset(join, SVSPP == "155") %>%
 # dplyr::distinct()

#redfish2 <- redfish %>%
 # dplyr::group_by(YEAR, SEASON) %>%
  #dplyr::summarise(min_lat = min(LAT),
   #                max_lat = max(LAT),
    #               min_lon = min(LON),
     #              max_lon = max(LON),
#  ) %>%
 # dplyr::mutate(range_lat = max_lat - min_lat,
  #              range_lon = max_lon - min_lon) %>%
#  tidyr::pivot_longer(cols = c("min_lat", "max_lat", "min_lon", "max_lon", "range_lat", "range_lon")) %>%
 # dplyr::ungroup()

##try for all species

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
                   max_lon = max(LON),) %>%
  dplyr::mutate(range_lat = max_lat - min_lat,
                range_lon = max_lon - min_lon) %>%
  tidyr::pivot_longer(cols = c("min_lat", "max_lat", "min_lon", "max_lon", "range_lat", "range_lon")) %>%
  dplyr::rename(indicator_name = name,
                indicator_value = value,
                species = COMNAME) %>%
  dplyr::ungroup()

##join all_tows and all_species
range <- rbind(all_tows, all_species) 

range <- range %>%
  dplyr::rename(INDICATOR_NAME = indicator_name,
                DATA_VALUE = indicator_value,
                SPECIES = species) %>%
  dplyr::mutate(INDICATOR_UNITS = "degrees")



#####################function###############

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
    }

range_test <- species_range(data = data, species)

