devtools::load_all()

load(here::here("data-raw/Survdat_2024.Rdata"))
test_2024 <- survdat |>
  dplyr::filter(stringr::str_detect(EST_TOWDATE, '2015-10-17'))

##current pull
channel <- dbutils::connect_to_database(server = 'NEFSC_pw_oraprod', uid = 'SOWEN')
test_survdat <- survdat::get_survdat_data(channel, getBio = TRUE, getLengths = TRUE)
test <- test_survdat$survdat
test_2025 <- test_survdat$survdat |>
  dplyr::filter(stringr::str_detect(EST_TOWDATE, '2015-10-17'))

#test

##2025 pull from neesp2 compare condition 
old_2025 <- readRDS(here::here("data-raw/condition.rds"))$survdat

test_old25 <- old_2025 |>
  dplyr::filter(stringr::str_detect(EST_TOWDATE, '2015-10-17'))

####################################################

# using a recent survdat pull:
all_condition_2025 <- test |>
  NEesp2::species_condition(
    LWparams = NEesp2::LWparams,
    species.codes = NEesp2::species.codes,
    output = "soe"
  )

#remove 2024 data from current pull, still has ~50 points that aren't present in 2024 data from condition package
test_2024_removed <- all_condition_2025 |>
  dplyr::filter(Time <= 2023)

# using an older survdat pull:
load(here::here("data-raw/Survdat_2024.Rdata"))
all_condition_2024 <- survdat |>
  NEesp2::species_condition(
    LWparams = NEesp2::LWparams,
    species.codes = NEesp2::species.codes,
    output = "soe"
  )


full_data <- ecodata::condition |>
  dplyr::rename(ecodata_value = Value) |>
  dplyr::full_join(
    all_condition_2024 |>
      dplyr::rename(new_value = Value)
  ) |>
  dplyr::full_join(
    all_condition_2025 |>
      dplyr::rename(new_value_2025 = Value))


dev_data <- full_data |>
  dplyr::mutate(dev_squared = (ecodata_value - new_value)^2,
                dev_squared_2024 = (ecodata_value - new_value_2025)^2)

plot_by_time(data = dev_data, grouping = c("Time", "EPU"), facet = "EPU")

species_dat <- dev_data |>
  dplyr::filter(!is.na(Var)) |>
  dplyr::mutate(species_group = stringr::str_detect(Var, "^[A-L]"))

fig1 <- species_dat |>
  dplyr::filter(species_group) |>
  plot_by_single(single = "Var") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
  ggplot2::ylim(c(0, 0.0042))

fig2 <- species_dat |>
  dplyr::filter(!species_group) |>
  plot_by_single(single = "Var") +
  ggplot2::theme(plot.title = ggplot2::element_blank()) +
  ggplot2::ylim(c(0, 0.0042))

ggpubr::ggarrange(fig1, fig2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "top")
