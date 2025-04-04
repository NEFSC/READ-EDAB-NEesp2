#' Plot swept area estimates from `survdat`
#'
#' This function plots `swept area estimates from `survdat` data.
#'
#' @param x A data frame or tibble, containing data on one species. The swept area estimate from `survdat`.
#' @param var The variable to plot ("abundance" or "biomass")
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_swept <- function(x, var) {
  if (var == "biomass") {
    x <- x %>%
      dplyr::rename(
        "value" = .data$tot.biomass,
        "error" = .data$tot.bio.SE
      )
    name <- "Survey biomass estimate (kg)"
  }
  
  if (var == "abundance") {
    x <- x %>%
      dplyr::rename(
        "value" = .data$tot.abundance,
        "error" = .data$tot.abund.SE
      )
    
    name <- "Survey abundance estimate"
  }
  
  if (nrow(x) > 0) {
    fig <- ggplot2::ggplot(x) +
      ggplot2::geom_ribbon(ggplot2::aes(
        x = .data$YEAR,
        ymin = .data$value - 2 * .data$error,
        ymax = .data$value + 2 * .data$error,
        fill = .data$Season
      ),
      alpha = 0.5
      ) +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$YEAR,
        y = .data$value,
        color = .data$Season
      ),
      cex = 2
      ) +
      nmfspalette::scale_color_nmfs("regional web") +
      nmfspalette::scale_fill_nmfs("regional web") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = name,
        labels = scales::comma
      )
    
    y <- x %>%
      dplyr::filter(is.na(.data$value) == FALSE) %>%
      dplyr::group_by(.data$Season) %>%
      dplyr::summarise(n_points = length(.data$value)) %>%
      dplyr::filter(.data$n_points >= 30)
    
    if (nrow(y) > 0) {
      fig <- fig +
        ecodata::geom_gls(ggplot2::aes(
          x = .data$YEAR,
          y = .data$value,
          color = .data$Season
        ))
    }
    
    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot species condition (weight/length^3)
#'
#' This function creates a plot of species condition (weight/length^3).
#'
#' @param x  A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_cond <- function(x) {
  data <- x %>%
    dplyr::filter(
      is.na(.data$pdlen) == FALSE,
      is.na(.data$pdwgt) == FALSE
    ) %>%
    dplyr::select(.data$pdlen, .data$pdwgt, .data$season, .data$geoarea, .data$fish_id, .data$year) %>%
    dplyr::distinct() %>% # remove duplicates
    dplyr::group_by(.data$geoarea, .data$season) %>%
    dplyr::mutate(n_fish = length(.data$pdlen)) %>%
    dplyr::filter(.data$n_fish > 10) # only region-season with >10 fish
  
  if (nrow(data) > 0) {
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$year,
        y = .data$pdwgt / (.data$pdlen^3)
      )
    ) +
      ggplot2::geom_jitter(
        alpha = 0.5,
        color = nmfspalette::nmfs_palette("regional web")(1)
      ) +
      ggplot2::ylab("Weight / Length^3 (g/cm^3)") +
      ggplot2::xlab("Year") +
      ggplot2::labs(title = "Condition factor") +
      ggplot2::theme_bw()
    
    if (unique(data$season) %>% length() > 1) {
      fig <- fig +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data$season),
          rows = ggplot2::vars(.data$geoarea)
        )
    } else {
      fig <- fig +
        ggplot2::facet_grid(rows = ggplot2::vars(.data$geoarea))
    }
    
    ecodat <- data %>%
      dplyr::group_by(.data$year, .data$season, .data$geoarea) %>%
      dplyr::summarise(mean_condition = mean(.data$pdwgt / (.data$pdlen^3))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$season, .data$geoarea) %>%
      dplyr::mutate(n_year = length(.data$year)) %>%
      dplyr::filter(.data$n_year > 30)
    
    if (length(ecodat$year) > 1) {
      fig <- fig +
        ecodata::geom_gls(
          inherit.aes = FALSE,
          data = ecodat,
          mapping = ggplot2::aes(
            x = .data$year,
            y = .data$mean_condition
          )
        )
    }
    
    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot diet composition
#'
#' This function plots the proportional composition of a species' diet.
#'
#' @param data A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_diet_plot <- function(data) {
  if (nrow(data) > 0) {
    normalized <- data %>%
      dplyr::filter(.data$pyamtw > 0) %>%
      
      # only look at season/year combinations with >20 predator samples
      dplyr::group_by(.data$year, .data$season, .data$geoarea) %>%
      dplyr::mutate(n_predators = .data$gensci %>%
                      unique() %>%
                      length()) %>%
      dplyr::filter(.data$n_predators > 20)
    
    if (length(normalized$n_predators) > 1) {
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(total_weight = sum(.data$pyamtw)) %>%
        dplyr::mutate(proportion = .data$total_weight / sum(.data$total_weight))
      
      normalized$gensci <- stringr::str_replace(normalized$gensci, " ", "_")
      
      # group low abundance prey as "other"
      groups <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(max_prop = max(.data$proportion)) %>%
        dplyr::filter(.data$max_prop > 0.05)
      
      groups <- groups$gensci %>% unique()
      
      rows <- match(normalized$gensci, groups) %>%
        is.na() %>%
        which()
      
      normalized[rows, "gensci"] <- "OTHER"
      
      # re-group proportions with new "other" category
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$geoarea, .data$gensci) %>%
        dplyr::summarise(prop2 = sum(.data$proportion))
      
      # add in zeros
      combo <- expand.grid(
        year = min(normalized$year):max(normalized$year),
        season = unique(normalized$season),
        Region = unique(normalized$geoarea),
        gensci = unique(normalized$gensci)
      )
      
      new_normalized <- dplyr::full_join(normalized, combo,
                                         by = c(
                                           "year" = "year",
                                           "season" = "season",
                                           "Region" = "Region",
                                           "gensci" = "gensci"
                                         )
      ) %>%
        dplyr::mutate(prop2 = ifelse(is.na(.data$prop2), 0, .data$prop2))
      
      # get order of most important - least important prey
      prey <- new_normalized %>%
        dplyr::group_by(.data$gensci) %>%
        dplyr::summarise(imp = max(.data$prop2)) %>%
        dplyr::arrange(dplyr::desc(.data$imp))
      
      new_normalized$gensci <- factor(
        new_normalized$gensci,
        prey$gensci
      )
      
      # assign colors based on nmfs color palette
      plot_colors <- NEesp::prey_palette$color
      names(plot_colors) <- NEesp::prey_palette$prey_id
      
      # plot
      fig <- ggplot2::ggplot(
        new_normalized,
        ggplot2::aes(
          x = .data$year,
          y = .data$prop2,
          fill = .data$gensci
        )
      ) +
        ggplot2::geom_bar(color = "black", stat = "identity") +
        ggplot2::scale_fill_manual(
          name = "Prey \ncategory",
          values = plot_colors
        ) +
        ggplot2::theme_classic() +
        ggplot2::ylab("Proportion of gut content") +
        ggplot2::theme(legend.position = "bottom")
      
      if (length(unique(new_normalized$gensci)) > 5) {
        fig <- fig +
          ggplot2::guides(fill = ggplot2::guide_legend(
            nrow = 2,
            byrow = TRUE,
            title = "Category"
          ))
      } else {
        fig <- fig +
          ggplot2::guides(title = "Category")
      }
      
      if (length(unique(new_normalized$season)) > 1) {
        fig <- fig +
          ggplot2::facet_grid(
            rows = ggplot2::vars(.data$season),
            cols = ggplot2::vars(.data$geoarea)
          )
      } else {
        fig <- fig +
          ggplot2::facet_grid(cols = ggplot2::vars(.data$geoarea))
      }
      
      print(fig)
    } else {
      print("NOT ENOUGH DATA")
    }
  } else {
    print("NO DATA")
  }
}

