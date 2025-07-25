#' Utility function to group by
#'
#' This function groups by year or by year and state
#'
#' @param data the data (a tibble)
#' @param groupby Boolean, if TRUE will group by STATE and YEAR, if false will group by YEAR only

groupby_state <- function(data, groupby) {
  if (groupby) {
    data <- data |>
      dplyr::group_by(.data$YEAR, .data$STATE)
  } else {
    data <- data |>
      dplyr::group_by(.data$YEAR)
  }

  return(data)
}

#' Read in MRIP recreational catch data
#'
#' This function read in MRIP catch data
#'
#' @param spceies the species common name
#' @param dir the directory where MRIP catch files are saved
#' @return a tibble
#' @export

## TODO: not sure this function is needed anymore

read_rec_catch <- function(species, dir, type = "all") {
  new_species <- species |>
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "_")

  files <- list.files(
    path = dir,
    full.names = TRUE
  )

  this_file <- files[which(stringr::str_detect(stringr::str_to_upper(files),
    pattern = paste0(
      stringr::str_to_upper(type),
      "_",
      stringr::str_to_upper(species)
    ) |>
      stringr::str_replace_all(" ", "_")
  ))]

  # read in the data
  rec_catch <- readRDS(this_file) |>
    purrr::map(~ janitor::clean_names(.x[1],
      case = "all_caps"
    ))

  return(rec_catch)
}

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
#' @return a tibble
#' @export

## TODO: rename to reflect that this works for any MRIP catch pull (not just total catch)

create_total_rec_catch <- function(data,
                                   # species,
                                   var_name = "catch",
                                   var_units = "n",
                                   remove_non_standard = TRUE) {
  total_rec_catch <- data |>
    janitor::clean_names(case = "all_caps") |>
    dplyr::rename_with(~"data_value",
      .cols = dplyr::matches("^TOTAL_.{1,15}$")
    ) |>
    dplyr::rename_with(~"keep",
      .cols = dplyr::starts_with("DOES")
    )

  if (remove_non_standard) {
    total_rec_catch <- total_rec_catch |>
      dplyr::filter(keep != "NO")
    message("Removing data that does not meet MRIP standards. If you want to keep this data, set `remove_non_standard = FALSE`.")
    if (nrow(total_rec_catch) == 0) {
      message("No data met MRIP standards; returning an empty tibble")
    }
  }

  output <- tibble::tibble(
    YEAR = total_rec_catch$YEAR,
    DATA_VALUE = total_rec_catch$data_value |> stringr::str_remove_all(",") |> as.numeric(),
    CATEGORY = "Recreational",
    INDICATOR_TYPE = "Socioeconomic",
    INDICATOR_NAME = paste0("total_recreational_", var_name, "_", var_units),
    INDICATOR_UNITS = var_units,
    # bring in species with data pull
    SPECIES = total_rec_catch$SPECIES
  )

  return(output)
}
# create_total_rec_catch(dat2$DATA, species = "atlantic cod")

# create_total_rec_catch <- function(data,
#                                    states = c('MAINE',
#                                               'CONNECTICUT',
#                                               'MASSACHUSETTS',
#                                               'NEW HAMPSHIRE',
#                                               'NEW JERSEY',
#                                               'NEW YORK',
#                                               'RHODE ISLAND',
#                                               'MARYLAND',
#                                               'DELAWARE',
#                                               'NORTH CAROLINA'),
#                                    groupby_state = FALSE,
#                                    return = TRUE){
#   total_rec_catch <- data %>%
#     dplyr::rename(tot_cat = TOTAL_CATCH_A_B1_B2) %>%
#     dplyr::filter(STATE %in% states) |>
#     groupby_state(groupby = groupby_state) |>
#     dplyr::summarise(DATA_VALUE = sum(tot_cat, na.rm = TRUE)) %>%
#     dplyr::mutate(CATEGORY = "Recreational",
#                   INDICATOR_TYPE = "Socioeconomic",
#                   INDICATOR_NAME = "total_recreational_catch_n",
#                   INDICATOR_UNITS = "lbs") %>%
#     # dplyr::rename(YEAR = Year,
#     #               STATE = State) %>% #remove STATE = State if wanting all states summed
#     dplyr::ungroup()## %>%
#     # dplyr::mutate(YEAR = as.numeric(YEAR))
#
#   if(return) return(total_rec_catch)
# }


#' Get MRIP trips file list
#'
#' This function returns a list of MRIP trip files
#' @param dir A directory that has subfolders with species-level data
#' @param species the species of interest
#' @return A vector of files
#' @export

get_trip_files <- function(dir, species) {
  new_dir <- list.dirs(dir,
    full.names = TRUE
  )
  this_dir <- new_dir[which(stringr::str_detect(stringr::str_to_upper(new_dir),
    pattern = stringr::str_to_upper(species) |>
      stringr::str_replace_all(" ", "_")
  ))]

  files <- list.files(this_dir,
    pattern = "[0-9].Rds",
    full.names = TRUE
  )

  return(files)
}

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
#' @return Saves R object `rec_trips`, returns directed recreational trips indicator
#' @export
# `%>%` <- magrittr::`%>%`


create_rec_trips <- function(files,
                             remove_non_standard = TRUE) {
  # rec_directed_trips <- c()
  # for (i in files) {
  #   this_dat <- readRDS(i)
  #   rec_directed_trips <- rbind(rec_directed_trips, this_dat)
  # } # TODO: convert this to purrr::reduce
  rec_trips <- files |>
    purrr::map(readRDS) |>
    purrr::map(purrr::pluck("data")) |>
    purrr::map(~ .x |>
      janitor::clean_names(case = "all_caps") |>
      dplyr::mutate(DIRECTED_TRIPS = stringr::str_remove_all(DIRECTED_TRIPS, ",") |>
        as.numeric()) |>
      dplyr::select(YEAR, DIRECTED_TRIPS, REGION, SPECIES, DOES_DIRECTED_TRIPS_MEET_MRIP_STANDARD)) |>
    purrr::reduce(dplyr::bind_rows)

  if (remove_non_standard) {
    rec_trips <- rec_trips |>
      dplyr::filter(DOES_DIRECTED_TRIPS_MEET_MRIP_STANDARD != "No")
  }

  output <- rec_trips |>
    dplyr::group_by(YEAR, SPECIES) |>
    dplyr::summarise(DATA_VALUE = sum(DIRECTED_TRIPS, na.rm = TRUE)) |>
    dplyr::mutate(
      CATEGORY = "Recreational",
      INDICATOR_TYPE = "Socioeconomic",
      INDICATOR_NAME = "rec_trips",
      INDICATOR_UNITS = "number"
    ) |>
    dplyr::select(YEAR, DATA_VALUE, CATEGORY, INDICATOR_TYPE, INDICATOR_NAME, INDICATOR_UNITS, SPECIES)

  return(output)
}

# create_rec_trips <- function(files,
#                              states = c('MAINE',
#                                         'CONNECTICUT',
#                                         'MASSACHUSETTS',
#                                         'NEW HAMPSHIRE',
#                                         'NEW JERSEY',
#                                         'NEW YORK',
#                                         'RHODE ISLAND',
#                                         'MARYLAND',
#                                         'DELAWARE',
#                                         'NORTH CAROLINA'),
#                              groupby_state = FALSE,
#                              return = TRUE) {
#   rec_directed_trips <- c()
#   for (i in files) {
#     this_dat <- read.csv(i,
#                          skip = 44,# was 24 is now 44
#                          na.strings = "."
#     )
#     # message(unique(this_dat$Year))
#     rec_directed_trips <- rbind(rec_directed_trips, this_dat)
#   }
#
#   rec_trips <- rec_directed_trips %>%
#     janitor::clean_names(case = "all_caps") %>%
#     dplyr::filter(STATE %in% states) %>%
#     groupby_state(groupby = groupby_state) |>
#     dplyr::summarise(DATA_VALUE = sum(DIRECTED_TRIPS, na.rm = TRUE)) %>%
#     dplyr::mutate(CATEGORY = "Recreational",
#                   INDICATOR_TYPE = "Socioeconomic",
#                   INDICATOR_NAME = "rec_trips",
#                   INDICATOR_UNITS = "n")
#
#   if(return) return(rec_trips)
#
# }

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
#' @return Saves the R data object `prop_sp_trips`
#' @export
# `%>%` <- magrittr::`%>%`

create_prop_sp_trips <- function(total_trips,
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
                                 return = TRUE) {
  total_trips <- total_trips |>
    dplyr::filter(STATE %in% states) |>
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(total_trips = sum(as.numeric(ANGLER_TRIPS), na.rm = TRUE)) |>
    dplyr::mutate(YEAR = as.numeric(YEAR))

  sp <- species_trips |>
    dplyr::filter(STATE %in% states) |>
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(as.numeric(DATA_VALUE), na.rm = TRUE))

  prop_sp_trips <- dplyr::full_join(total_trips,
    sp,
    by = "YEAR"
  ) |>
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


#' Create MRIP recreational landings indicator
#'
#' This function creates an indicator for total recreational landings
#' For new data queries, use MRIP Query Tool (https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries)
#' Query 'Time Series' under 'Catch Data'.
#' Choose years of interest, summarize by Annual, Calendar Year, Atlantic Coast by State, species of interest, all modes and areas, Harvest (A + B1), choose # of fish/weight (lbs), mean length and weight
#' Download csv as output
#'
#' @param data The MRIP harvest data (R object `mrip_landing`)
#' @param states States in which to filter data, from MRIP query 'ATLANTIC COAST BY STATE'
#' @importFrom magrittr %>%
#' @return Saves the R data object `total_rec_landings`
#' @export
# `%>%` <- magrittr::`%>%`

## TODO: I think this function can be deprecated

create_total_rec_landings <- function(data,
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
                                      return = TRUE) {
  total_rec_landings <- data %>%
    dplyr::rename(lbs_ab1 = HARVEST_A_B1_TOTAL_WEIGHT_LB) %>%
    dplyr::filter(STATE %in% states) %>%
    groupby_state(groupby = groupby_state) |>
    dplyr::summarise(DATA_VALUE = sum(lbs_ab1, na.rm = TRUE)) %>%
    dplyr::mutate(
      CATEGORY = "Recreational",
      INDICATOR_TYPE = "Socioeconomic",
      INDICATOR_NAME = "total_recreational_landings_lbs",
      INDICATOR_UNITS = "lbs"
    ) %>%
    # dplyr::rename(YEAR = Year,
    #               STATE = State) %>% #remove STATE = State here if want all states summed
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))

  if (return) {
    return(total_rec_landings)
  }
}
