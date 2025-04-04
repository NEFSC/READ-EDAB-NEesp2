#' Plot indicator time series 
#'
#' This function plots an indicator time series with a year, indicator name, and data value with mean and standard deviation lines.
#'
#' @param data A data frame with columns of YEAR, INDICATOR_NAME, and DATA_VALUE
#' @param ar Aspect Ratio for the resulting plot
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' 
plt_indicator <- function(data,
                          ar = 1/4) {
  plt <- data |>
    dplyr::group_by(INDICATOR_NAME) |>
    dplyr::mutate(mean = mean(DATA_VALUE, na.rm = TRUE),
                  sd = sd(DATA_VALUE, na.rm = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = YEAR,
                                 y = DATA_VALUE
    )) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean + .data$sd
    ),
    color = "darkgreen",
    linetype = "solid"
    ) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean - .data$sd
    ),
    color = "darkgreen",
    linetype = "solid"
    ) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean
    ),
    color = "darkgreen",
    linetype = "dotted"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_blank(),
                   aspect.ratio = ar,
                   plot.background = ggplot2::element_rect(fill='transparent'))
  
  return(plt)
}

# png(here::here("inst/esp_template/05_images/test.png"),
#     bg = "transparent",
#     width = 6,
#     height = 1.5,
#     units = "in",
#     res = 72)
# plt_indicator(dat)
# dev.off()
