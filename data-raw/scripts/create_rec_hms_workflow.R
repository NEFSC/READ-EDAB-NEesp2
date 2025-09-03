#' Creates recreational HMS indicator
#'
#' This uses the output file from 'pull_rec_hms' as an input parameter
#' The function formats the data to be used in the SOE
#' It is formatted exactly like the ecodata data object
#'
#' @param inputFile Character string. Full path to the data from the 'pull_rec_hms' csv file. 
#' @param key ?????
#'
#' @examples
#' \dontrun{
#' # create the ecodata::rec_hms indicator
#' create_rec_hms(
#   inputFile = here::here("data-raw/hms_mrip/2025-08-26/hms_mrip_2025-08-26.csv"))
#'
#' }
#' 
#' @return rec.hms, ecodata::rec_hms data frame
#'
#' @export


create_rec_hms <- function(
    inputFile,
    key = NEesp2::hms_key # this could be hardcoded into the function, rather than passed as a parameter
) {
  # data wrangling ----
  rec_hms <- read.csv(inputFile) |>
    dplyr::left_join(
      key |> dplyr::select(COMMON_NAME, SP_CATEGORY),
      by = c("SPECIES" = "COMMON_NAME")
    ) |>
    dplyr::group_by(YEAR, SP_CATEGORY, REGION) |>
    dplyr::summarise(
      Value = sum(DATA_VALUE)
    ) |>
    dplyr::rename(
      Var = SP_CATEGORY,
      Time = YEAR,
      EPU = REGION
    ) |>
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
  
  return(rec_hms)
}