#' Create a flextable with inset indicator time series plots
#'
#' This function creates a flextable for ESP report cards.
#'
#' @param data A data frame or tibble
#' @return A flextable
#' @export

rpt_card_table <- function(data,
                           widths = c(0.9, 0.75, 3, 3)) {
  
  # if(sum(!colnames(data) %in% c("figure", "w", "h", "indicator", "status.+", "implications")) != 0) {
  #   stop("Your input data is missing columns or has incorrect column names")
  # }
  
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
                          width = widths,
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

#' Format data from report card spreadsheet 
#'
#' This function formats the data to prep for flextable creation.
#'
#' @param file the file path
#' @param term_year the terminal year to use in the table header
#' @param dir the directory where the time series images are saved
#' @return A tibble
#' @export


format_tbl_data <- function(file,
                            term_year,
                            dir) {
  if(stringr::str_detect(file, "csv$")) {
    out <- read.csv(file)
  } else if(stringr::str_detect(file, "xlsx$")) {
    out <- readxl::read_excel(file)
  } else {
    stop("File must be a .csv or .xlsx")
  }
  
  output <- out |>
    janitor::clean_names() |>
    dplyr::mutate(figure =  paste0(dir, "/", time_series)) |>
    dplyr::rename_with(.fn = ~paste(.x, "in", term_year, sep = "_"),
                       .cols = "status") |>
    dplyr::select(-time_series)
  
  return(output)
}
