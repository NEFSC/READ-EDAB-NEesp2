#' Convert longform tidy ESP indicator data to netcdf
#'
#' This function convertd longform tidy ESP indicator data to netcdf
#'
#' @param data A data frame containing ESP indicator data in long format. Must contain columns: INDICATOR_NAME, UNITS, DATA_VALUE, YEAR, and SUBMISSION_YEAR
#' @param fname A character string specifying the file path where the .nc file will be saved
#' @return .nc file saved at the specified filepath
#' @export

esp_csv_to_nc <- function(
  data,
  fname
) {
  var.index <- data |>
    dplyr::select(INDICATOR_NAME, UNITS) |>
    dplyr::distinct()

  years <- sort(unique(data$YEAR))

  data2 <- data |>
    # expand to include missing years
    dplyr::full_join(expand.grid(
      YEAR = years,
      INDICATOR_NAME = unique(data$INDICATOR_NAME)
    )) |>
    dplyr::arrange(YEAR)

  # Define dimensions
  time_dim <- ncdf4::ncdim_def(name = "year", units = "years", vals = years)

  # Define variables
  var.ls <- purrr::map2(
    var.index$INDICATOR_NAME,
    var.index$UNITS,
    ~ ncdf4::ncvar_def(
      name = .x,
      units = .y,
      dim = list(time_dim),
      missval = -999,
      longname = .x,
      prec = "float"
    )
  )

  # Create NetCDF file
  nc_file <- ncdf4::nc_create(
    filename = fname,
    vars = var.ls
  )

  # Add global attributes
  global_attr <- data |>
    dplyr::select(-c("YEAR", "DATA_VALUE", "INDICATOR_NAME", "UNITS")) |>
    dplyr::distinct() |>
    tidyr::drop_na("SUBMISSION_YEAR") |>
    dplyr::mutate_all(as.character) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "attribute",
      values_to = "value"
    ) |>
    dplyr::group_by(attribute) |>
    dplyr::mutate(num_vals = dplyr::n_distinct(value)) |>
    dplyr::filter(num_vals == 1)

  if (nrow(global_attr) > 0) {
    purrr::walk2(
      global_attr$attribute,
      global_attr$value,
      ~ {
        ncdf4::ncatt_put(nc_file, 0, .x, .y)
      }
    )
  }

  # Puts all the values in
  for (j in 1:nrow(var.index)) {
    all_data <- data2 |>
      dplyr::filter(INDICATOR_NAME == var.index$INDICATOR_NAME[j])

    var.data <- all_data |>
      dplyr::select(DATA_VALUE) |>
      as.matrix()

    var.data[which(is.na(var.data))] <- -999

    # Write data to file
    ncdf4::ncvar_put(nc_file, var.ls[[j]], var.data)
    # print(var.ls[[j]])

    # Add attributes to the variable
    attr_data <- all_data |>
      dplyr::select(
        -c("YEAR", "DATA_VALUE", "INDICATOR_NAME", "UNITS")
      ) |>
      tidyr::drop_na("SUBMISSION_YEAR") |>
      dplyr::distinct() |>
      dplyr::select(-global_attr$attribute)

    if (nrow(attr_data) > 0) {
      purrr::walk(
        colnames(attr_data),
        ~ {
          ncdf4::ncatt_put(
            nc_file,
            var.ls[[j]],
            .x,
            unique(attr_data[[.x]])
          )
        }
      )
    }
  }

  # Close the NetCDF file
  ncdf4::nc_close(nc_file)
}

## input data = long format indicator time series of all variables

# data <- AKesp::get_esp_data("Black Sea Bass") |>
#   dplyr::filter(INDICATOR_NAME != "BSB_Winter_Bottom_Temperature_North")
#
# esp_csv_to_nc(data = data, fname = here::here("data-raw", "bsb_example3.nc"))

# Check the contents of the NetCDF file

# nc_file_check <- ncdf4::nc_open(here::here("data-raw", "bsb_example3.nc"))
#
# ncdf4::ncatt_get(nc_file_check, 0)
#
# ncdf4::ncvar_get(nc_file_check, "BSB_Shelf_Water_Volume_North")
