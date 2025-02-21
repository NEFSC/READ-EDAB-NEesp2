#' Plot Condition 
#'
#' @param data data frame with condition data from bottom trawl survey
#' @param species species of interest to plot
#' @return A ggplot

plot_condition <- function(data, 
                           var = c(
                             'Acadian redfish', 'American plaice', 'Atlantic cod',
                             'Atlantic croaker', 'Atlantic herring', 'Atlantic mackerel',
                             'Atlantic menhaden', 'Atlantic wolffish', 'Barndoor skate',
                             'Black sea bass', 'Blackbelly rosefish', 'Bluefish',
                             'Bluntnose stingray', 'Bullnose ray', 'Butterfish',
                             'Clearnose skate', 'Cusk', 'Fourspot', 'Goosefish',
                             'Haddock', 'Little skate', 'Longhorn sculpin',
                             'Ocean pout', 'Offshore hake', 'Pollock',
                             'Red hake', 'Rosette skate', 'Roughtail stingray',
                             'Scup', 'Sea raven', 'Silver hake', 'Smooth dogfish',
                             'Smooth skate', 'Spiny butterfly ray', 'Spiny dogfish',
                             'Spot', 'Spotted hake', 'Summer flounder',
                             'Thorny skate', 'Weakfish', 'White hake',
                             'Windowpane', 'Winter flounder', 'Winter skate',
                             'Witch flounder', 'Yellowtail flounder'),
                           return = TRUE){
  condition <- data |>
    dplyr::select(Time = YEAR,
                  Var = Species,
                  EPU,
                  Value = MeanCond,
                  nCond) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(scaleCond = scale(Value,scale =T,center=T))
  
  xs <- quantile(condition$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)
  
  condition <- condition |>
    dplyr::mutate(category = cut(scaleCond,
                                 breaks = xs,
                                 labels = c( "Poor Condition",
                                             "Below Average",
                                             "Neutral",
                                             "Above Average",
                                             "Good Condition"),
                                 include.lowest = TRUE))
  
  condition <- condition |>
    dplyr::filter(Var %in% var) |>
    dplyr::ungroup() |>
    dplyr::arrange(Time) |>
    dplyr::group_by(EPU) |>
    dplyr::mutate(mean = mean(Value, na.rm = TRUE),
                  sd = sd(Value, na.rm = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = Time,
                                 y = Value,
                                 color = category,
                                 shape = EPU
    )) +
    ggplot2::geom_path(color = "black", lty = 2, alpha = 0.5) +
    ggplot2::geom_point(cex = 3) +
    ggplot2::xlim(c(1989, 2024)) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_blank(),
                   aspect.ratio = 0.4,
                   legend.direction = "vertical",
                   legend.box = "horizontal") +
    viridis::scale_color_viridis(discrete = TRUE)
  
  return(condition)
}

###### testing #####
#AnnualRelCond2023_Fall <- readr::read_csv("https://raw.githubusercontent.com/NOAA-EDAB/foodweb-risk/main/condition/AnnualRelCond2023_Fall.csv")
#plot_condition(data = AnnualRelCond2023_Fall, var = 'Black sea bass')

                  
  