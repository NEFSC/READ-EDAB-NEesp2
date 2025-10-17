channel <- dbutils::connect_to_database(server = 'NEFSC_pw_oraprod', uid = 'SOWEN')
survdat_data <- survdat::get_survdat_data(channel, getBio = TRUE, getLengths = TRUE)

data <- survdat_data$survdat
ecodata_condition <- ecodata::condition

survdat_condition <- NEesp2::species_condition(
    data = survdat_data$survdat,
    LWparams = NEesp2::LWparams,
    species.codes = NEesp2::species.codes,
    by_EPU = TRUE,
    by_sex = FALSE,
    length_break = NULL,
    output = "soe")


conditionData <- readRDS(here::here('data-raw/conditionData.rds'))
conditionData <- conditionData$survdat

test <- create_condition(inputPath = here::here('data-raw/conditionData.rds'),
                         inputPathLW = here::here('data-raw/LWparams.csv'),
                         inputPathSpecies = here::here('data-raw/species.codes.csv'))


####################################################
full_data <- ecodata_condition |>
  dplyr::rename(ecodata_value = Value) |>
  dplyr::full_join(
    test |>
      dplyr::rename(new_value = Value))

dev_data <- full_data |>
  dplyr::mutate(dev = (ecodata_value - new_value)^2)

dev_data <- saveRDS(dev_data, here::here('data-raw/condition_dev_data.rds'))



