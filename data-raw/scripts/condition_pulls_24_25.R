channel <- dbutils::connect_to_database(server = 'NEFSC_pw_oraprod', uid = 'SOWEN')
current_survdat <- survdat::get_survdat_data(channel, getBio = TRUE, getLengths = TRUE)

survdat <- current_survdat$survdat
ecodata_condition <- ecodata::condition

survdat_condition <- NEesp2::species_condition(
    data = survdat,
    LWparams = NEesp2::LWparams,
    species.codes = NEesp2::species.codes,
    by_EPU = TRUE,
    by_sex = FALSE,
    length_break = NULL,
    output = "soe")



condition_from_network_drive <- readRDS(here::here('data-raw/condition.rds'))

full_data <- ecodata_condition |>
  dplyr::rename(ecodata_value = Value) |>
  dplyr::full_join(
    survdat_condition |>
      dplyr::rename(survdat_value = Value)
  ) |>
  dplyr::full_join(
    condition_from_network_drive |>
      dplyr::rename(network_drive_value = Value))

dev_data <- full_data |>
  dplyr::mutate(dev_ecodata_survdat = (ecodata_value - survdat_value)^2,
                dev_ecodata_network = (ecodata_value - network_drive_value)^2)

dev_data <- saveRDS(dev_data, here::here('data-raw/condition_dev_data.rds'))



