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



