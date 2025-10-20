#' Create MRIP total recreational catch indicator
#'
#' This function creates the total recreational catch indicator
#' Input data is MRIP catch (A, B1, B2 catch combined)
#'
#' @param data The mrip data
#' @param species The species common name
#' @param var_name The variable name to use in the indicator name. Default is "catch".
#' @param var_units The variable units to use in the indicator name. Default is "n".
#' @param remove_non_standard Boolean, if TRUE will remove non-standard data ("Does Total Catch (A+B1+B2) Meet MRIP Standard" = NO)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a tibble
#' @export

create_total_mrip <- function(
  data,
  # species,
  var_name = "catch",
  var_units = "n",
  remove_non_standard = TRUE
) {
  total_rec_catch <- data |>
    janitor::clean_names(case = "all_caps") |>
    dplyr::rename_with(
      ~"data_value",
      .cols = dplyr::matches("^TOTAL_.{1,15}$")
    ) |>
    dplyr::rename_with(~"keep", .cols = dplyr::starts_with("DOES"))

  if (remove_non_standard) {
    total_rec_catch <- total_rec_catch |>
      dplyr::filter(.data$keep != "NO")
    message(
      "Removing data that does not meet MRIP standards. If you want to keep this data, set `remove_non_standard = FALSE`."
    )
    if (nrow(total_rec_catch) == 0) {
      message("No data met MRIP standards; returning an empty tibble")
    }
  }

  output <- tibble::tibble(
    YEAR = total_rec_catch$YEAR,
    DATA_VALUE = total_rec_catch$data_value |>
      stringr::str_remove_all(",") |>
      as.numeric(),
    LOWER_95_CI = total_rec_catch |>
      dplyr::select(dplyr::contains("LOWER")) |>
      dplyr::pull() |>
      stringr::str_remove_all(",") |>
      as.numeric(),
    UPPER_95_CI = total_rec_catch |>
      dplyr::select(dplyr::contains("UPPER")) |>
      dplyr::pull() |>
      stringr::str_remove_all(",") |>
      as.numeric(),
    CATEGORY = "Recreational",
    INDICATOR_TYPE = "Socioeconomic",
    INDICATOR_NAME = paste0("total_recreational_", var_name, "_", var_units),
    INDICATOR_UNITS = var_units,
    # bring in species with data pull
    SPECIES = total_rec_catch$SPECIES,
    REGION = total_rec_catch$REGION
  )

  return(output)
}
# create_total_rec_catch(dat2$DATA, species = "atlantic cod")

#' Get MRIP trips file list

#' Create MRIP total recreational trips indicator
#'
#' This function creates a total recreational trips indicator
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Directed Trip' under 'Effort Data'.
#' Choose year of interest, summarize by Annual, Calendar Year, Atlantic coast by state, species of interest, all modes and areas, Primary Target
#' Download csv as output
#' @param files A list of the full file names of annual directed trip data (.csv format). Must download for each year in MRIP query tool.
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Saves R object `rec_trips`, returns directed recreational trips indicator
#' @export

create_mrip_trips <- function(files, remove_non_standard = TRUE) {
  rec_trips <- files |>
    purrr::map(readRDS) |>
    purrr::map(purrr::pluck("data")) |>
    purrr::map(
      ~ .x |>
        janitor::clean_names(case = "all_caps") |>
        dplyr::mutate(
          DIRECTED_TRIPS = stringr::str_remove_all(DIRECTED_TRIPS, ",") |>
            as.numeric()
        ) |>
        dplyr::select(
          YEAR,
          DIRECTED_TRIPS,
          REGION,
          SPECIES,
          DOES_DIRECTED_TRIPS_MEET_MRIP_STANDARD
        )
    ) |>
    purrr::reduce(dplyr::bind_rows)

  if (remove_non_standard) {
    rec_trips <- rec_trips |>
      dplyr::filter(.data$DOES_DIRECTED_TRIPS_MEET_MRIP_STANDARD != "No")
  }

  output <- rec_trips |>
    dplyr::group_by(.data$YEAR, .data$SPECIES) |>
    dplyr::summarise(DATA_VALUE = sum(.data$DIRECTED_TRIPS, na.rm = TRUE)) |>
    dplyr::mutate(
      CATEGORY = "Recreational",
      INDICATOR_TYPE = "Socioeconomic",
      INDICATOR_NAME = "rec_trips",
      INDICATOR_UNITS = "number"
    ) |>
    dplyr::select(
      .data$YEAR,
      .data$DATA_VALUE,
      .data$CATEGORY,
      .data$INDICATOR_TYPE,
      .data$INDICATOR_NAME,
      .data$INDICATOR_UNITS,
      .data$SPECIES
    )

  return(output)
}

#' Create MRIP species recreational effort indicator
#'
#' This function creates an indicator with the proportion trips targeting a species of interest
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Effort Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, all modes and areas
#' Download csv as output
#'
#' @param total_trips The MRIP trip data (R object `mrip_effort`) of total annual angler trips from downloaded csv file
#' @param species_trips The directed trips data for species of interest (R object `rec_trips`) from function 'create_rec_trips'
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Saves the R data object `prop_sp_trips`
#' @export

## TODO: needs to be updated to work with new format of output,
## function should be updated to reflect that the total_trips and species_trips need to contain data at the same regional level

create_prop_sp_trips <- function(
  total_trips,
  species_trips,
  states = c(
    "MAINE",
    "CONNECTICUT",
    "MASSACHUSETTS",
    "NEW HAMPSHIRE",
    "NEW JERSEY",
    "NEW YORK",
    "RHODE ISLAND",
    "MARYLAND",
    "DELAWARE",
    "NORTH CAROLINA"
  ),
  groupby_state = FALSE,
  return = TRUE
) {
  total_trips <- total_trips |>
    dplyr::filter(.data$STATE %in% states) |>
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(
      total_trips = sum(as.numeric(.data$ANGLER_TRIPS), na.rm = TRUE)
    ) |>
    dplyr::mutate(YEAR = as.numeric(.data$YEAR))

  sp <- species_trips |>
    dplyr::filter(.data$STATE %in% states) |>
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(as.numeric(.data$DATA_VALUE), na.rm = TRUE))

  prop_sp_trips <- dplyr::full_join(total_trips, sp, by = "YEAR") |>
    dplyr::mutate(
      DATA_VALUE = DATA_VALUE / total_trips,
      INDICATOR_NAME = "proportion_sp_trips",
      INDICATOR_UNITS = "%"
    ) |>
    dplyr::select(-total_trips) |>
    # comment out the following lines if wanting all states summed
    dplyr::ungroup()
  # dplyr::select(-Year)
  # dplyr::rename(STATE = State)

  if (return) {
    return(prop_sp_trips)
  }
}
