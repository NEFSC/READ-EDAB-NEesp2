#' Create a flextable with inset indicator time series plots
#'
#' This function creates a flextable for ESP report cards.
#'
#' @param data A data frame or tibble
#' @return A flextable
#' @export

rpt_card_table <- function(data) {
  
  if(sum(!colnames(data) %in% c("figure", "w", "h", "indicator_units", "status_in_2024", "implications")) != 0) {
    stop("Your input data is missing columns or has incorrect column names")
  }
  
  small_dat <- data |>
    dplyr::mutate(figure = NA) |>
    dplyr::select(-c(w, h)) |>
    dplyr::rename(time_series = figure)
  
  colnames(small_dat) <- colnames(small_dat) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
  
  flextable::set_flextable_defaults(font.size = 10)
  tbl <- flextable::flextable(small_dat) 
  tbl <- flextable::width(tbl, j = 1:4, 
                          width = params$widths,
                          unit = "in")
  tbl <- flextable::theme_box(tbl)
  
  for(i in 1:nrow(small_dat)){
    tbl <- flextable::compose(tbl,
                              i = i,
                              j = 4,
                              value = flextable::as_paragraph(
                                flextable::as_image(src = data$figure[i],
                                                    width = data$w[i],
                                                    height = data$h[i],
                                                    unit = "in",
                                                    guess_size = FALSE)))
  }
  
  tbl <- flextable::align(tbl, i = 1, j = 1:4, align = "center", part = "header")
  tbl <- flextable::align(tbl, i = 1:nrow(small_dat), j = 4, align = "center")
  
  return(tbl)
}
