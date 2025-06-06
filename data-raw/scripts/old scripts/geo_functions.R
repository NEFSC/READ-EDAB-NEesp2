#' Create a map of seasonal species distribution
#'
#' This function creates a map of species distribution. Built from the `ecodata` function `map_strata`.
#'
#' @param strata A data frame or tibble, containing data on one species. Must contain season and strata information.
#' @param common_name The common name of the species.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

map_strata <- function(common_name, strata) {
  if (nrow(strata) > 0) {
    ## General mapping parameters
    xmin <- -77
    xmax <- -65
    ymin <- 35
    ymax <- 45

    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
    crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    ## Download data layers

    ## 2) North America layer
    ne_countries <- rnaturalearth::ne_countries(
      scale = 10,
      continent = "North America",
      returnclass = "sf"
    ) %>%
      sf::st_transform()

    ## 3) State layer
    ne_states <- rnaturalearth::ne_states(
      country = "united states of america",
      returnclass = "sf"
    ) %>%
      sf::st_transform()

    # strata
    strata_spring <- strata %>%
      dplyr::filter(.data$stock_season == "spring") %>%
      dplyr::select(.data$strata, .data$stock_season) %>%
      dplyr::rename("spring" = .data$stock_season)

    strata_fall <- strata %>%
      dplyr::filter(.data$stock_season == "fall") %>%
      dplyr::select(.data$strata, .data$stock_season) %>%
      dplyr::rename("fall" = .data$stock_season)

    strata_winter <- strata %>%
      dplyr::filter(.data$stock_season == "winter") %>%
      dplyr::select(.data$strata, .data$stock_season) %>%
      dplyr::rename("winter" = .data$stock_season)

    # overlapping strata
    all_season <- dplyr::full_join(strata_spring, strata_fall,
      by = "strata"
    )

    all_season <- dplyr::full_join(all_season, strata_winter, by = "strata")

    all_season <- all_season[, colSums(is.na(all_season)) < nrow(all_season)] # Remove rows with NA only

    label <- all_season %>%
      dplyr::select(-strata)

    labelv <- c()
    for (i in seq_len(nrow(label))) {
      labelv[i] <- paste(label[i, ], collapse = ", ")
    }

    all_season$label <- labelv

    all_season <- all_season %>%
      dplyr::select(.data$strata, .data$label) %>%
      dplyr::rename(STRATA = strata) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(label = .data$label %>%
        stringr::str_replace_all(", NA", "") %>%
        stringr::str_replace_all("NA, ", ""))

    # For plotting
    new_shape <- NEesp::shape %>%
      dplyr::select(.data$STRATA, .data$geometry) %>%
      sf::st_transform()

    sf::st_crs(new_shape) <- crs

    strata_plot <- dplyr::full_join(new_shape, all_season, by = "STRATA") %>%
      dplyr::rename(SEASON = label) %>%
      dplyr::filter(!is.na(.data$SEASON))

    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = new_shape, # all trawl shape files (light outlines)
        fill = "white",
        alpha = 0.9,
        size = 0.01,
        color = "grey30"
      ) +
      ggplot2::geom_sf(
        data = strata_plot, # occupied trawl shape files (filled with color)
        ggplot2::aes(fill = .data$SEASON),
        size = 0.05,
        color = "grey30"
      ) +
      ggplot2::geom_sf(
        data = ne_countries,
        color = "grey60",
        size = 0.25
      ) +
      ggplot2::geom_sf(
        data = ne_states,
        color = "grey60",
        size = 0.05
      ) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggplot2::coord_sf(
        crs = crs,
        xlim = xlims,
        ylim = ylims
      ) +
      ggthemes::theme_map() +
      ggplot2::labs(
        title = common_name,
        fill = "Season"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.key.width = ggplot2::unit(2, "cm")
      )

    return(p1)
  } else {
    print("NO DATA")
  }
}

#' Create a summary table of species range
#'
#' This function creates a summary table of species distribution (latitude/longitude range).
#'
#' @param data A data frame or tibble, containing data on one species. Must contain season and strata information.
#' @param x The common name of the species.
#' @param shapefile A shapefile to convert strata ID to a shape.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_latlong <- function(x, data, shapefile) {
  range_coord <- c()

  data <- data %>%
    dplyr::filter(.data$Species == x)

  for (i in unique(data$Region)) {
    for (j in unique(data$stock_season)) {
      data2 <- data %>%
        dplyr::filter(.data$stock_season == j, .data$Region == i)

      if (nrow(data2) > 0) {
        log_statement <- paste("STRATA == ", unique(data2$strata), collapse = " | ")

        strata <- dplyr::filter(shapefile, eval(parse(text = log_statement)))

        temp <- c(i, j, round(sf::st_bbox(strata), digits = 2))

        missing_data <- match(unique(data2$strata), unique(shapefile$STRATA)) %>%
          is.na() %>%
          sum()

        if (missing_data == 0) {
          warning <- "none"
        }
        if (missing_data > 0) {
          warning <- "shapefile is missing some strata data"
        }

        range_coord <- rbind(range_coord, c(temp, warning))
      }
    }
  }

  if (length(range_coord) > 0) {
    colnames(range_coord) <- c(
      "Region", "Season", "Lat_min",
      "Long_min", "Lat_max", "Long_max", "Warning"
    )
  }

  return(range_coord)
}
