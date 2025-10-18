#' Create an ESP template
#'
#' This function creates an ESP two-pager template at the specified path.
#' @param path Where to create the template. Defaults to the present working directory.
#' @param over whether to overwrite existing files. Defaults to FALSE.
#' @export

create_template <- function(path = getwd(), over = FALSE) {
  file.copy(
    from = list.files(
      path = system.file(c("esp_template"), package = "NEesp2"),
      # recursive = TRUE,
      full.names = TRUE
    ),
    to = path,
    recursive = TRUE,
    overwrite = over
  )
}

# create_template(here::here("data-raw/temp"), over = TRUE)
