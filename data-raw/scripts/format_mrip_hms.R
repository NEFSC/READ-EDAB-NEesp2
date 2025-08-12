library(magrittr)
library(dplyr)
## try to process hms similar to ecodata

new_hms <- read.csv(here::here("data-raw/hms_mrip/hms_mrip_2025-08-04.csv"))

hms_key <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ecodata/refs/heads/master/data-raw/hms-mrip/hms_sp_category.csv")

rec_hms <- new_hms |>
  dplyr::left_join(hms_key |> dplyr::select(COMMON_NAME, SP_CATEGORY),
    by = c("SPECIES" = "COMMON_NAME")
  ) |>
  # will have to update to pull mid and NE separately
  dplyr::mutate(EPU = "ALL") |>
  dplyr::group_by(YEAR, SP_CATEGORY, EPU) |>
  dplyr::summarise(Value = sum(DATA_VALUE)) |>
  dplyr::rename(Var = SP_CATEGORY) |>
  dplyr::rename(Time = YEAR) |>
  dplyr::mutate(
    Var = paste0(Var, "-", EPU),
    Region = EPU
  ) |>
  dplyr::select(Time, Var, Value, EPU) |>
  dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request (MRIP should not be used for pelagics)

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

## original code from ecodata

rec_hms <- d %>%
  dplyr::filter(SUB_REG <= 5) %>% # 4 = North Atlantic, 5 = Mid atlantic
  left_join(sp_cat, by = "SP_CODE") %>% # merge catch year with common names and category
  dplyr::group_by(YEAR, SP_CATEGORY, SUB_REG) %>%
  dplyr::summarise(Value = sum(LANDING)) %>% #  Definition of Landings. The total number of fish removed from the fishery resource.
  # May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
  dplyr::rename(
    Time = YEAR,
    Var = SP_CATEGORY,
    Region = SUB_REG
  ) %>%
  dplyr::mutate(Region = as.character(Region)) %>%
  dplyr::mutate(
    EPU = dplyr::recode(Region,
      `4` = "NE",
      `5` = "MAB"
    ),
    Region = dplyr::recode(Region,
      `4` = "New England",
      `5` = "Mid-Atlantic"
    )
  ) %>%
  dplyr::mutate(Var = paste0(Var, "-", EPU)) %>%
  dplyr::select(Time, Var, Value, EPU) |>
  dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request (MRIP should not be used for pelagics)
