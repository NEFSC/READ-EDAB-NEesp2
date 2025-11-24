#' Create Spatial Indicator
#'
#' This function generates a spatially-aggregated indicator from a netCDF file.
#' Converts .nc files to data frame.
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Parameters passed to EDABUtilities::make_2d_summary_ts are specified in the `...` argument and listed below:
#' data.in = Either a character vector of full input file names for a list of spatRasters
#' file.time = What time scale the input files are on ('daily','monthly','annual')? Assumes all monthly or annual files are on a daily timestep
#' output.files = vector of full output file names corresponding to each input file
#' shp.file = Shape file you wish to crop each input file to
#' var.name = Variable name you wish to extract.
#' area.names = Names of shape file areas you want to summarize. 
#' statistic = Which statistic to calculate = 'mean'
#' agg.time = Time scale to calculate over (days, doy, months, season, or years)
#' tz = Time zone to convert. No correction if NA
#' touches = If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' write.out = If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @param indicator_name name of the indicator (character)
#' @param units units associated with the indicator (character)
#' @param ... passed to `EDABUtilities::make_2d_summary_ts`
#' @return Returns a data frame summarized by timestep for each area.names
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

create_spatial_indicator <- function(indicator_name,
                                     units,
                                     ...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  
  output <- df %>%
    dplyr::mutate(time = as.Date(.data$time),
                  DAY = lubridate::day(.data$time), 
                  MONTH = lubridate::month(.data$time), 
                  YEAR = lubridate::year(.data$time)) %>%
    subset(select = -c(.data$agg.time, .data$time, .data$ls.id, .data$var.name) ) |>
    dplyr::mutate(INDICATOR_NAME = indicator_name,
                  INDICATOR_UNITS = units)  %>%
    dplyr::rename(DATA_VALUE = .data$value,
                  AREA = .data$area,
                  STATISTIC = .data$statistic) %>%
    purrr::discard(~all(is.na(.)))
  
  return(output)
}
