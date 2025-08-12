###############################################

devtools::load_all()

species_list <- c(
  'atlantic angel shark',
  'atlantic sharpnose shark',
  'basking shark',
  'bigeye thresher',
  'bignose shark',
  'blacknose shark',
  'blacktip shark',
  'blue shark',
  'bonnethead',
  'bull shark',
  'dusky shark',
  'finetooth shark',
  'great hammerhead',
  'lemon shark',
  'night shark',
  'nurse shark',
  'oceanic whitetip',
  'oceanic whitetip shark',
  'porbeagle',
  'sand tiger',
  'sandbar shark',
  'scalloped hammerhead shark',
  'sevengill shark',
  'shortfin mako',
  'silky shark',
  'sixgill shark',
  'smooth hammerhead',
  'spinner shark',
  'thresher shark',
  'tiger shark',
  'white shark'
)

region_list <- c('north atlantic', 'mid-atlantic')

query_params <- expand.grid(species = species_list, regions = region_list)

data_pull <- purrr::map2(
  query_params$species,
  query_params$regions,
  ~ {
    save_data <- save_catch(
      this_species = .x,
      this_region = .y,
      this_data_type = "numbers of fish",
      out_folder = here::here("data-raw/hms_mrip/2025-08-04"),
      catch_type = "landings",
      wait = FALSE
    )

    data <- readRDS(save_data)

    if (is.data.frame(data$data)) {
      create_total_mrip(
        data$data,
        var_name = "hms_landings",
        var_units = "n",
        remove_non_standard = FALSE
      )
    }
  }
)

output <- purrr::reduce(data_pull, dplyr::bind_rows)
write.csv(
  output,
  here::here("data-raw/hms_mrip/hms_mrip_2025-08-04.csv"),
  row.names = FALSE
)

########################################
## Format to ecodata formatting

new_hms <- read.csv(here::here("data-raw/hms_mrip/hms_mrip_2025-08-04.csv"))

hms_key <- read.csv(
  "https://raw.githubusercontent.com/NOAA-EDAB/ecodata/refs/heads/master/data-raw/hms-mrip/hms_sp_category.csv"
)

rec_hms <- new_hms |>
  dplyr::left_join(
    hms_key |> dplyr::select(COMMON_NAME, SP_CATEGORY),
    by = c("SPECIES" = "COMMON_NAME")
  ) |>
  dplyr::group_by(YEAR, SP_CATEGORY, REGION) |>
  dplyr::summarise(Value = sum(DATA_VALUE)) |>
  dplyr::rename(Var = SP_CATEGORY) |>
  dplyr::rename(Time = YEAR, EPU = REGION) |>
  dplyr::mutate(
    EPU = dplyr::case_when(
      EPU == "MID-ATLANTIC" ~ "MAB",
      EPU == "NORTH ATLANTIC" ~ "NE"
    )
  ) |>
  dplyr::mutate(
    Var = paste0(Var, "-", EPU)
  ) |>
  dplyr::select(Time, Var, Value, EPU) |>
  dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request


## Plot/compare to ecodata::rec_hms

#ecodata_hms <- ecodata::rec_hms

rec_hms |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = Var
  )) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

ecodata::rec_hms |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = Var
  )) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

rec_hms |>
  dplyr::mutate(source = "new") |>
  dplyr::bind_rows(
    ecodata::rec_hms |>
      dplyr::mutate(source = "ecodata")
  ) |>
  # dplyr::filter(Value > 0) |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = source,
    lty = source
  )) +
  ggplot2::geom_point(pch = 1) +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~Var, scales = "free_y", ncol = 2)


rec_hms |>
  dplyr::mutate(source = "new") |>
  dplyr::bind_rows(
    ecodata::rec_hms |>
      dplyr::mutate(source = "ecodata")
  ) |>
  dplyr::filter(
    Value > 0,
    Var == "LargeCoastal-MAB" | Var == "SmallCoastal-MAB",
    Time > 2010
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = source,
    lty = source
  )) +
  ggplot2::geom_point(pch = 1) +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~Var, scales = "free_y", ncol = 2) +
  ggplot2::scale_x_continuous(breaks = seq(2011, 2022, by = 2))
