###############################################

devtools::load_all()

## set up query ----
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
  'white shark',
  # billfishes
  'billfish family',
  'black marlin',
  'blue marlin',
  'shortbill spearfish',
  'striped marlin',
  # tunas -- not including mackerels and pacific fish
  'albacore',
  'bigeye tuna',
  'bluefin tuna',
  'skipjack tuna',
  'striped bonito',
  'wahoo',
  'yellowfin tuna'
)

region_list <- c('north atlantic', 'mid-atlantic')

query_params <- expand.grid(species = species_list, regions = region_list)

## query from mrip ----
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


## recalculating with confidence intervals

## recalculating with confidence intervals ----

files <- list.files(
  here::here("data-raw/hms_mrip/2025-08-04"),
  full.names = TRUE
)

data_pull <- purrr::map(
  files,
  ~ {
    data <- readRDS(.x)

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




## save output ----

output <- purrr::reduce(data_pull, dplyr::bind_rows)
write.csv(
  output,
  here::here("data-raw/hms_mrip/hms_mrip_2025-08-14.csv"),
  row.names = FALSE
)

########################################
## Format to ecodata formatting

new_hms <- read.csv(here::here("data-raw/hms_mrip/hms_mrip_2025-08-14.csv"))

hms_key <- read.csv(
  "https://raw.githubusercontent.com/NOAA-EDAB/ecodata/refs/heads/master/data-raw/hms-mrip/hms_sp_category.csv"
) |>
  dplyr::bind_rows(tibble::tibble(
    COMMON_NAME = c(
      'billfish family',
      'black marlin',
      'blue marlin',
      'shortbill spearfish',
      'striped marlin'
    ),
    SP_CATEGORY = "Billfishes"
  )) |>
  dplyr::bind_rows(tibble::tibble(
    COMMON_NAME = c(
      'albacore',
      'bigeye tuna',
      'bluefin tuna',
      'skipjack tuna',
      'striped bonito',
      'wahoo',
      'yellowfin tuna'
    ),
    SP_CATEGORY = "Scombridae"
  ))

rec_hms <- new_hms |>
  dplyr::left_join(
    hms_key |> dplyr::select(COMMON_NAME, SP_CATEGORY),
    by = c("SPECIES" = "COMMON_NAME")
  ) |>
  ## this is how we could calculate the variance if the data were normal and IID
  dplyr::mutate(variance = ((UPPER_95_CI - LOWER_95_CI) / 2 / 1.96)^2) |>
  dplyr::group_by(YEAR, SP_CATEGORY, REGION) |>
  dplyr::summarise(
    Value = sum(DATA_VALUE),
    Lower_bound = sum(LOWER_95_CI),
    Upper_bound = sum(UPPER_95_CI),
    sum_variance = sum(variance)
  ) |>
  dplyr::mutate(
    ## this is how we could calculate the confidence intervals if the data were normal and IID
    lower_95_ci = Value - 1.96 * sqrt(sum_variance),
    upper_95_ci = Value + 1.96 * sqrt(sum_variance)
  ) |>
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
  dplyr::select(Time, Var, Value, Lower_bound, Upper_bound, EPU) |>
  dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request


## Plot/compare to ecodata::rec_hms

### plot on their own
rec_hms |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = Var
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = Lower_bound, ymax = Upper_bound, fill = Var),
    alpha = 0.1,
    lty = 2
  ) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~Var, scales = "free_y", ncol = 2)

ecodata::rec_hms |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = Var
  )) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

### drop 0s
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

### look at 2014-2017 differences
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
