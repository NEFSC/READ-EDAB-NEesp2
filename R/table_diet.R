#' Create a table of diet data
#'
#' This function creates a table of the proportional composition of a species' diet.
#'
#' @param data A data frame or tibble, containing data on one species. Data from `allfh`.
#' @param type The file type of the output. One of c("html", "word")
#' @return A `DT::datatable` or a `knitr::kable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_diet_table <- function(data, type = "html") {
  if (nrow(data) > 0) {
    normalized <- data %>%
      dplyr::filter(.data$pyamtw > 0) %>%
      
      # only look at season/year combinations with >20 predator samples
      dplyr::group_by(.data$year, .data$season, .data$Region) %>%
      dplyr::mutate(n_predators = .data$fish_id %>%
                      unique() %>%
                      length()) %>%
      dplyr::filter(.data$n_predators > 20)
    
    if (length(normalized$n_predators) > 1) {
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(total_weight = sum(.data$pyamtw)) %>%
        dplyr::mutate(proportion = .data$total_weight / sum(.data$total_weight))
      
      normalized$gensci <- stringr::str_replace(normalized$gensci, " ", "_")
      
      # group low abundance prey as "other"
      groups <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(max_prop = max(.data$proportion)) %>%
        dplyr::filter(.data$max_prop > 0.05)
      
      groups <- groups$gensci %>%
        unique()
      
      rows <- match(normalized$gensci, groups) %>%
        is.na() %>%
        which()
      
      normalized[rows, "gensci"] <- "OTHER"
      
      # re-group proportions with new "other" category
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(prop2 = sum(.data$proportion))
      
      # summary table
      table <- normalized %>%
        dplyr::group_by(.data$gensci, .data$season, .data$Region, .data$year) %>%
        dplyr::filter(sum(.data$prop2) > 0) %>%
        dplyr::group_by(.data$gensci, .data$season, .data$Region) %>%
        dplyr::summarise(
          mean_proportion = paste(mean(.data$prop2) %>% round(digits = 3),
                                  " +- ",
                                  stats::sd(.data$prop2) %>% round(digits = 3),
                                  " (", length(.data$prop2), ") ",
                                  sep = ""
          ),
          
          range_proportion = paste(min(.data$prop2) %>% round(digits = 3),
                                   max(.data$prop2) %>% round(digits = 3),
                                   sep = " - "
          )
        )
      
      make_html_table(table,
                      type = type,
                      col_names = c(
                        "Prey category", "Season", "Region",
                        "Mean proportion +- SD (n years)",
                        "Range"
                      )
      )
    } else {
      print("NOT ENOUGH DATA")
    }
  } else {
    print("NO DATA")
  }
}