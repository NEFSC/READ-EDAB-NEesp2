#' Create an ESP template
#'
#' This function creates an ESP template at the specified path. DEPRECATED -- PLAN TO REMOVE
#' @param path Where to create the template. Defaults to the present working directory.
#' @export


create_template <- function(path = getwd(),
                            over = FALSE) {
  file.copy(
    from = list.files(path = system.file(c("esp_template"),
                                         package = "NEesp2"), 
                      # recursive = TRUE,
                      full.names = TRUE),
    to = path,
    recursive = TRUE,
    overwrite = over
  )
}

# create_template(here::here("data-raw/temp"), over = TRUE)
