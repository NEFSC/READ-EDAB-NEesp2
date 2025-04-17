
## create strata-epu key ----

strata_epu_key <- tibble::tibble(STRATUM = list(c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510),
                                      c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550),
                                      c(1220, 1240, 1260:1290, 1360:1400, 3560:3830),
                                      c(1300:1352, 1401:1599, 3840:3990)),
                       EPU = c("MAB", "GB", "GOM", "SS")) |>
  tidyr::unnest(cols = STRATUM)

usethis::use_data(strata_epu_key)

## create lw params ----

LWparams <- readr::read_csv(here::here("data-raw/tech_memo_parameters_table_format.csv"))

#Standardize syntax of Condition L-W data for merge with survey data:
# LWparams <- readr::read_csv(here::here("data-raw/tech_memo_parameters_table_format.csv"))
LWparams <- dplyr::mutate(LWparams,
                           lna1 = substr(ln_a, 2, nchar(ln_a)),
                           lna = as.numeric(lna1)*-1) |>
  dplyr::mutate(SEASON = dplyr::case_when(Season == 'Autumn' | 
                                            Season == 'Win/Aut' | 
                                            Season == 'Spr/Aut' |
                                            Season == 'Win/Spr/Aut' ~ "FALL",
                                          Season == 'Win/Spr' |
                                            Season == 'Spring' ~ 'SPRING',
                                          Season == 'Winter' ~ 'WINTER',
                                          TRUE ~ NA))

usethis::use_data(LWparams, overwrite = TRUE)

## create species codes ----

species.codes <- utils::read.csv(here::here("data-raw/bottomtrawl_species_codes_names.csv"))
usethis::use_data(species.codes)
