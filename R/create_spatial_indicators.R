## Create Spatial Indicator Functions

#' Create Spatial Indicator
#'
#' This function generates a spatially-aggregated indicator from a netCDF file. Has been tested with GLORYS netCDF and OCCCI ERDDAP netCDF. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param indicator_name name of the indicator (character)
#' @param units units associated with the indicator (character)
#' @param ... passed to `EDABUtilities::make_2d_summary_ts`
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract.
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Returns a data frame summarized by timestep for each area.names
#' @importFrom magrittr %>%
#' @export

create_spatial_indicator <- function(indicator_name,
                                     units,
                                     ...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  
  output <- df %>%
    dplyr::mutate(time = as.Date(time),
                  DAY = lubridate::day(time), 
                  MONTH = lubridate::month(time), 
                  YEAR = lubridate::year(time)) %>%
    subset(select = -c(agg.time, time, ls.id, var.name) ) |>
    dplyr::mutate(INDICATOR_NAME = indicator_name,
                  INDICATOR_UNITS = units)  %>%
    dplyr::rename(DATA_VALUE = value,
                  AREA = area,
                  STATISTIC = statistic) %>%
    purrr::discard(~all(is.na(.)))
  
  return(output)
}

#' Create Sea Surface Temperature Indicator
#'
#' This function generates a sea surface temperature indicator from an OISST ERDDAP netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' **NOTE: OISST DATA DOES NOT WORK WITH EDAB_UTILS. FUNCTION WILL RUN BUT ALL 'DATA_VALUE' WILL BE NA
#' Converts .nc files to data frame.
#' @param ... passed to `EDABUtilities::make_2d_summary_ts()`
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'sst'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters'
#' @return Saves R object `sst`, returns sea surface temp indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr %>%
#' @export

create_sst <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  
  sst <- df %>%
    dplyr::mutate(time = as.Date(time),
                  DAY = lubridate::day(time), 
                  MONTH = lubridate::month(time), 
                  YEAR = lubridate::year(time)) %>%
    dplyr::mutate(INDICATOR_UNITS = c ('degC')) %>%
    subset(select = -c(agg.time, time, ls.id) ) %>%
    dplyr::rename(INDICATOR_NAME = var.name,
                  DATA_VALUE = value,
                  AREA = area,
                  STATISTIC = statistic) %>%
  
  return(sst)
}


#' Create Chlorophyll-a Indicator
#'
#' This function generates a chlorophyll-a indicator from an OCCCI ERDDAP netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' **NOTE: OCCCI DATA DOES NOT WORK WITH EDAB_UTILS. FUNCTION WILL RUN BUT ALL 'DATA_VALUE' WILL BE NA
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'chl'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `chl`, returns chlorophyll-a indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr %>%
#' @export


create_chl <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
 
   df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  
   chl <- df %>%
    dplyr::mutate(time = as.Date(time),
                  DAY = lubridate::day(time), 
                  MONTH = lubridate::month(time), 
                  YEAR = lubridate::year(time)) %>%
    dplyr::mutate(INDICATOR_UNITS = c ('mg m^-3')) %>%
     subset(select = -c(agg.time, time, ls.id))  %>%
     dplyr::rename(INDICATOR_NAME = var.name,
                   DATA_VALUE = value,
                   AREA = area,
                   STATISTIC = statistic) 
  
  return(chl)
}
