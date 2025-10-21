#' Plot Condition 
#'
#' @param data A data frame of species condition from `species_condition(..., output = "soe")`
#' @param var species of interest to plot
#' @importFrom rlang .data
#' @return A ggplot
#' @export

plot_condition <- function(data, 
                           var,
                           return = TRUE){
  condition <- data |>
    dplyr::select(.data$YEAR,
                  .data$Species,
                  .data$EPU,
                  .data$DATA_VALUE) |>
    dplyr::group_by(.data$Species) |>
    dplyr::mutate(scaleCond = scale(.data$DATA_VALUE,scale =T,center=T))
  
  xs <- stats::quantile(condition$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)
  
  condition <- condition |>
    dplyr::mutate(category = cut(.data$scaleCond,
                                 breaks = xs,
                                 labels = c( "Poor Condition",
                                             "Below Average",
                                             "Neutral",
                                             "Above Average",
                                             "Good Condition"),
                                 include.lowest = TRUE))
  
  condition <- condition |>
    dplyr::filter(.data$Species %in% var) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$YEAR) |>
    dplyr::group_by(.data$EPU) |>
    dplyr::mutate(mean = mean(.data$DATA_VALUE, na.rm = TRUE),
                  sd = stats::sd(.data$DATA_VALUE, na.rm = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YEAR,
                                 y = .data$DATA_VALUE,
                                 color = .data$category,
                                 shape = .data$EPU
    )) +
    ggplot2::geom_path(color = "black", lty = 2, alpha = 0.5) +
    ggplot2::geom_point(cex = 3) +
    ggplot2::xlim(c(1989, 2024)) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 16),
                   axis.title.x = ggplot2::element_blank(),
                   aspect.ratio = 0.4,
                   legend.direction = "vertical",
                   legend.box = "horizontal") +
    viridis::scale_color_viridis(discrete = TRUE)
  
  return(condition)
}


