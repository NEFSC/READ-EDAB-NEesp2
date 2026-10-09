old_data <- read.csv(here::here("data-raw/plaice_commercial_data_old.csv"))
new_data <- read.csv(here::here("data-raw/plaice_commercial_data_new.csv"))


diff_rows <- dplyr::anti_join(new_data, old_data)
print(diff_rows)


comparison <- dplyr::inner_join(
  old_data,
  new_data,
  by = c("YEAR", "CATEGORY", "INDICATOR_NAME", "INDICATOR_TYPE"),
  suffix = c("_old", "_new")
) |>
  dplyr::filter(DATA_VALUE_old != DATA_VALUE_new) |>
  dplyr::select(YEAR, INDICATOR_NAME, DATA_VALUE_old, DATA_VALUE_new)
