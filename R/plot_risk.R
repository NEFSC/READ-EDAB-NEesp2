#' Plot Risk Assessment summary 
#'
#' @param risk_elements A tibble of the stock's risk assessment results (number of risk elements at each level, by category. Results should be ordered from Low to High)
#' @return A ggplot
#' 
#' @export

plot_risk <- function(risk_elements = tibble::tibble(stock = c(1, 5, 2, 0),
                                                     commercial = c(3, 2, 2, 0),
                                                     recreational = c(2, 3, 0, 1))
) {
  
  data <- risk_elements |>
    dplyr::mutate(level = rep(c("Low", "Lowmod", "Modhigh", "High"))) |>
    tidyr::pivot_longer(cols = -level) |>
    dplyr::group_by(name) |>
    dplyr::mutate(percent = value/sum(value),
                  lab = ifelse(value != 0,
                               paste(value, level),
                               NA),
                  pos = cumsum(percent),
                  xlab = paste0(stringr::str_to_title(name),
                         "\n(", sum(value), " Risk Elements)"))
  
  data$level = factor(data$level, levels = rev(c("Low", "Lowmod", "Modhigh", "High")))

  plt <-
     data |>
    ggplot2::ggplot(ggplot2::aes(x = xlab,
                                y = percent,
                                # color = level, 
                                fill = level)) +
    ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(y = pos - 0.05,
                                       label = lab)) +
    ggplot2::geom_text(ggplot2::aes(y = -0.1,
                                    label = xlab)) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Percent of Risk Elements") +
      ggplot2::scale_y_continuous(labels = scales::label_percent()) +
      ggplot2::scale_fill_brewer(palette = "RdYlGn") +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(color = "gray"),
                     axis.text.y = ggplot2::element_text(color = "black"),
                     axis.text.x = ggplot2::element_blank())
  
  return(plt)
}
