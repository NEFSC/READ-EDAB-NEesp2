
#' Locate files containing a text string
#'
#' This function locates all .R and .Rmd files that contain a text string.
#'
#' @param text A text string to search for.
#' @param path The folder to search (searches recursively). Defaults to the project root.
#' @return A vector of names of files that contain the string.
#'
#' @importFrom magrittr %>%
#' @export

find_files <- function(text, path = here::here()) {
  all_files <- c(
    list.files(path, recursive = TRUE, full.names = TRUE) %>%
      stringr::str_subset("\\.R$"),
    list.files(path, recursive = TRUE, full.names = TRUE) %>%
      stringr::str_subset("\\.Rmd$")
  )

  out <- c()

  for (i in seq_len(length(all_files))) {
    results <- grep(text, readLines(all_files[i]), value = FALSE) %>% suppressWarnings()

    if (length(results) > 0) {
      results <- paste(results, collapse = ", ")

      this_data <- c(all_files[i], results)

      out <- rbind(out, this_data)
    }

    percent <- (i / length(all_files) * 100) %>%
      round(digits = 0)

    if ((i %% 10) == 0) {
      print(paste(i, " files searched, ", percent, "% done", ".....", sep = ""))
    }
  }

  if (is.null(out)) {
    print("Not found")
  } else {
    colnames(out) <- c("file", "line(s)")
    return(out)
  }
}
