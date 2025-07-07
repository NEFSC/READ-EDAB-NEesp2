#catch

bsb_catch <- get_mrip_catch(species = "BLACK SEA BASS", type = "all")
catch <- bsb_catch$data |>
  dplyr::rename(YEAR = Year,
                DATA_VALUE = `Total Catch (A+B1+B2)`) |>
  dplyr::mutate(INDICATOR_NAME = 'Recreational Catch')

plt_indicator(catch)

#landings

bsb_landings <- get_mrip_catch(species = "BLACK SEA BASS", type = "landings")
landings <- bsb_landings$data |>
  dplyr::rename(YEAR = Year,
                DATA_VALUE = `Total Harvest (A+B1)`) |>
  dplyr::mutate(INDICATOR_NAME = 'Recreational Landings')

plt_indicator(landings)

#trips

trips_2020 <- get_mrip_trips(species = "BLACK SEA BASS", region = "North Atlantic", year = '2020')
trips_2021 <- get_mrip_trips(species = "BLACK SEA BASS", region = "North Atlantic", year = '2021')
trips_2022 <- get_mrip_trips(species = "BLACK SEA BASS", region = "North Atlantic", year = '2022')
trips_2023 <- get_mrip_trips(species = "BLACK SEA BASS", region = "North Atlantic", year = '2023')
trips_2024 <- get_mrip_trips(species = "BLACK SEA BASS", region = "North Atlantic", year = '2024')

trips2_2020 <- get_mrip_trips(species = "BLACK SEA BASS", region = "Mid-Atlantic", year = '2020')
trips2_2021 <- get_mrip_trips(species = "BLACK SEA BASS", region = "Mid-Atlantic", year = '2021')
trips2_2022 <- get_mrip_trips(species = "BLACK SEA BASS", region = "Mid-Atlantic", year = '2022')
trips2_2023 <- get_mrip_trips(species = "BLACK SEA BASS", region = "Mid-Atlantic", year = '2023')
trips2_2024 <- get_mrip_trips(species = "BLACK SEA BASS", region = "Mid-Atlantic", year = '2024')

trips <- rbind(trips_2020$data,
               trips_2021$data,
               trips_2022$data,
               trips_2023$data,
               trips_2024$data,
               trips2_2020$data,
               trips2_2021$data,
               trips2_2022$data,
               trips2_2023$data,
               trips2_2024$data)

trips <- trips |>
  dplyr::rename(YEAR = Year,
                DATA_VALUE = `Directed Trips`) |>
  dplyr::mutate(INDICATOR_NAME = 'Recreational Trips')

pak::pak("noaa-edab/ecodata") 
