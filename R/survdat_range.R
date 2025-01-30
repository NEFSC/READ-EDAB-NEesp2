`%>%` <- magrittr::`%>%`

## Pull survdat data
channel <- dbutils::connect_to_database("NEFSC_USERS","SOWEN")
data <- survdat::get_survdat_data(channel)

#filter survdat data to year, season, lat, and species code
survdat_data <- data$survdat %>%
  dplyr::select(SVSPP, YEAR, SEASON, LAT)

#get species names, filter to species code, scientific + common name
species <- survdat::get_species(channel)
species_data <- species$data %>%
  tidyr::drop_na(SVSPP) %>%
  dplyr::select (SVSPP, SCINAME, COMNAME)

#join survdat df with species df grouping by species code
join <- dplyr::full_join(survdat_data, species_data %>%
                   dplyr::group_by(SVSPP))

#test subsetting for one species (acadian redfish) 
redfish <- subset(join, SVSPP == "155") %>%
  dplyr::distinct()

##want to end up with 2 rows for each year (one for each season): year | season | min lat | max lat | range | species
##want species as a parameter in the function but probably not year/season? (likely would want all years/seasons)

# test min/max for year 2000
#min/max not in separate rows?
test <- redfish[redfish$YEAR %in% c('2000'),]
min <- test[which.min(test$LAT),] 
max <- test[which.max(test$LAT),] 
test_join <- dplyr::full_join(min, max %>%
                           dplyr::group_by(SVSPP))

#works kinda but keeps all the rows?
min <- test %>%
  dplyr::summarise(min(LAT))
test$min_lat <- c(min)

max <- test %>%
  dplyr::summarise(max(LAT))
test$max_lat <- c(max)
