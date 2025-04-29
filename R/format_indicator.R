#' Format data for upload to AKFIN web service
#'
#' This function formats indicator data for upload to AKFIN web service
#'
#' @param out_dir dictory where the .txt file will be saved
#' @param submission_year Current year of contribution submission. This is a integer value.
#' @param indicator_name Name of the indicator. This is a primary key and must match what has been used in previous years. This is a text string.
#' @param description Brief description of the indicator. Please make sure this description includes information on the spatial distribution of the indicator and how the data for the indicator are collected. The spatial resolution can be a cell size dimension (e.g., 5 km resolution for gridded data) or area of sampling for a survey. The data collection method can be brief (e.g., survey name and gear type, satellite sensor and version, stock assessment model output, fishery observer data, community reports, etc.) and can include a reference to methods detailed elswhere. This is a text string.
#' @param status Information on the current status of the indicator in the context of historical trends. This is a text string.
#' @param factors Information on the potential causes for observed trends and current status (focus on most recent year). This is a text string.
#' @param implications Information that briefly answers these questions: What are the implications or impacts of the observed trends on the ecosystem or ecosystem components? What do the trends mean? Why are they important? How can this information be used to inform management decisions? This is a text string.
#' @param references Include any full references that are associated with the indicator. This may include data references such as from an ERDDAP webpage or literature cited (plus the DOI where possible). This is a text string.
#' @param years Years for the indicator contribution. This is a numeric vector.
#' @param region List of spatial or temporal scales applicable to indicator. This is a character value.
#' @param indicator_value Data values for the indicator contribution (must match the YEAR list length). This is a numeric vector.
#' @param ... not used
#' @return A .txt file saved in `out_dir`
#' @export

format_indicator <- function(out_dir,
                             submission_year,
                             indicator_name,
                             description,
                             status,
                             factors,
                             implications,
                             references,
                             years,
                             region,
                             indicator_value,
                             ... # getting an error that there are unused arguments -- hacky fix
                             ) {
  filename <- paste0(out_dir, "/", indicator_name, ".txt")

  cat(
    "#Ecosystem and Socioeconomic Profile (ESP) indicator template for Northeast ESPs \n",
    "#This template is required for updating ESP indicator information. There are two required sections to check or update (see below): Indicator Review and Indicator Data. \n",
    '#INSTRUCTIONS: Please fill in the text (surrounded by " ") or data as values in the line after each field marked with a # and capitalized name. Help text is provided following the capitalized field name. \n',
    "#INDICATOR_REVIEW ----------------------------------------------------------------------------------------",
    "#SUBMISSION_YEAR - Current year of contribution submission. This is a integer value.",
    submission_year,
    "#INDICATOR_NAME - Name of the indicator - text string (unique to the indicator).",
    indicator_name,
    "#DESCRIPTION - Brief description of the indicator. Please make sure this description includes information on the spatial distribution of the indicator and how the data for the indicator are collected. The spatial resolution can be a cell size dimension (e.g., 5 km resolution for gridded data) or area of sampling for a survey. The data collection method can be brief (e.g., survey name and gear type, satellite sensor and version, stock assessment model output, fishery observer data, community reports, etc.) and can include a reference to methods detailed elswhere. This is a text string.",
    description,
    "#STATUS_TRENDS - Information on the current status of the indicator in the context of historical trends. This is a text string.",
    status,
    "#FACTORS - Information on the potential causes for observed trends and current status (focus on most recent year). This is a text string.",
    factors,
    "#IMPLICATIONS - Information that briefly answers these questions: What are the implications or impacts of the observed trends on the ecosystem or ecosystem components? What do the trends mean? Why are they important? How can this information be used to inform management decisions? This is a text string.",
    implications,
    "#REFERENCES - Include any full references that are associated with the indicator. This may include data references such as from an ERDDAP webpage or literature cited (plus the DOI where possible). This is a text string.",
    references,
    "\n #INDICATOR_DATA ----------------------------------------------------------------------------------------------",
    "#YEAR - List of years for the indicator contribution. This is a integer value.",
    paste(years, collapse = "\t"),
    "#REGION/SEASON - List of spatial or temporal scales applicable to indicator. This is a character value.",
    region,
    "#INDICATOR_VALUE - List of data values for the indicator contribution (must match the YEAR list length). This is a numeric value.",
    paste(indicator_value, collapse = "\t"),

    file = filename,
    sep = "\n"
  )
}


#' Wrapper function to format data for upload to AKFIN web service
#'
#' This function is a wrapper that allows a metadata data table to be used to format indicator data for upload to AKFIN web service
#'
#' @param key The metadata (a tibble)
#' @param ind_name Name of the indicator. This is a primary key and must match what has been used in previous years. This is a text string.
#' @param ... Passed to `NEesp2::format_indicator()`
#' @return A .txt file saved in `out_dir` (`out_dir` must be pased with `...`)
#' @export

format_from_template <- function(key,
                                 ind_name,
                                 ...) {
  this_dat <- key |>
    dplyr::filter(indicator_name == ind_name)

  format_indicator(indicator_name = ind_name,
                   description = this_dat$description,
                   status = this_dat$status,
                   factors = this_dat$factors,
                   implications = this_dat$implications,
                   references = this_dat$references,
                   region = this_dat$region,
                   ...
                   )
}

#' Simple function to add image file paths to indicator table template within targets pipeline
#'
#' This function adds image file paths to the template data table indicator table template. Allows for tracking of the images and table data within the targets pipeline.
#'
#' @param path The file path to the indicator table template (an .xlsx)
#' @param list_files Names of the images to include. This is a vector of text strings.
#' @return A tibble
#' @export

add_fig_paths <- function(path,
                          list_files) {

  output <- readxl::read_excel(path) |>
    dplyr::mutate(time_series = list_files)

  return(output)
}
