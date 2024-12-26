
#' Prepare recreational data for plotting
#'
#' This function does basic data manipulation on recreational data.
#'
#' @param data Recreational data for a single species. Subsetted from MRIP
#' @param state If TRUE, group data by state
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

rec_data_prep <- function(data, state = FALSE) {

  # for adding zeros
  combo <- expand.grid(
    year = min(data$year, na.rm = TRUE):max(data$year, na.rm = TRUE),
    mode_fx_f = unique(data$mode_fx_f)
  )

  # group by state if TRUE

  if (state) {
    data <- data %>%
      dplyr::group_by(.data$st_f)
  }

  # northeast data

  ne <- data %>%
    dplyr::filter(.data$sub_reg_f == "NORTH ATLANTIC" |
      .data$sub_reg_f == "MID-ATLANTIC") %>%
    dplyr::group_by(.data$mode_fx_f, .data$year, .add = TRUE) %>%
    dplyr::summarise(
      total_catch = sum(.data$tot_cat),
      discards = sum(.data$estrel),
      landings = sum(.data$lbs_ab1),
      landings_num = sum(.data$landing)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(
      total_catch_all_mode = sum(.data$total_catch),
      prop = .data$discards / .data$total_catch_all_mode,
      prop_land = .data$landings_num / .data$total_catch_all_mode
    ) %>%
    dplyr::full_join(combo,
      by = c(
        "year" = "year",
        "mode_fx_f" = "mode_fx_f"
      )
    ) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(.data$total_catch), 0, .data$total_catch)) %>%
    dplyr::select(-.data$total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "Northeast")


  # outside of northeast data

  out <- data %>%
    dplyr::filter(.data$sub_reg_f != "NORTH ATLANTIC" &
      .data$sub_reg_f != "MID-ATLANTIC") %>%
    dplyr::group_by(.data$mode_fx_f, .data$year) %>%
    dplyr::summarise(
      total_catch = sum(.data$tot_cat),
      discards = sum(.data$estrel),
      landings = sum(.data$lbs_ab1),
      landings_num = sum(.data$landing)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(
      total_catch_all_mode = sum(.data$total_catch),
      prop = .data$discards / .data$total_catch_all_mode,
      prop_land = .data$landings_num / .data$total_catch_all_mode
    ) %>%
    dplyr::full_join(combo,
      by = c(
        "year" = "year",
        "mode_fx_f" = "mode_fx_f"
      )
    ) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(.data$total_catch), 0, .data$total_catch)) %>%
    dplyr::select(-.data$total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "Outside\nNortheast")

  # total data

  all <- data %>%
    dplyr::group_by(.data$mode_fx_f, .data$year) %>%
    dplyr::summarise(
      total_catch = sum(.data$tot_cat),
      discards = sum(.data$estrel),
      landings = sum(.data$lbs_ab1),
      landings_num = sum(.data$landing)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(
      total_catch_all_mode = sum(.data$total_catch),
      prop = .data$discards / .data$total_catch_all_mode,
      prop_land = .data$landings_num / .data$total_catch_all_mode
    ) %>%
    dplyr::full_join(combo,
      by = c(
        "year" = "year",
        "mode_fx_f" = "mode_fx_f"
      )
    ) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(.data$total_catch), 0, .data$total_catch)) %>%
    dplyr::select(-.data$total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "All Regions")

  # combine

  full_data <- rbind(ne, out, all)

  # get order of most important - least important category
  cat <- full_data %>%
    dplyr::filter(.data$name == "landings") %>%
    dplyr::group_by(.data$mode_fx_f) %>%
    dplyr::summarise(imp = max(.data$value)) %>%
    dplyr::arrange(dplyr::desc(.data$imp))

  full_data$mode_fx_f <- factor(
    full_data$mode_fx_f,
    cat$mode_fx_f
  )

  return(full_data)
}

#' Plot recreational data
#'
#' This function plots recreational data.
#'
#' @param data Recreational data for a single species. Subsetted from MRIP
#' @param var What data to plot. One of c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")
#' @param title The title for the graph
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_rec <- function(data, var, title) {
  if (nrow(data) > 0) {
    data <- NEesp::rec_data_prep(data)

    # assign colors based on nmfs color palette
    plot_colors <- NEesp::rec_palette$color
    names(plot_colors) <- NEesp::rec_palette$rec_mode

    # plot
    fig <- ggplot2::ggplot(
      data %>%
        dplyr::filter(.data$name == var),
      ggplot2::aes(
        x = .data$year,
        y = .data$value,
        fill = .data$mode_fx_f
      )
    ) +
      ggplot2::geom_bar(color = "black", stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = "",
        labels = scales::comma
      ) +
      ggplot2::xlab("Year") +
      ggplot2::scale_fill_manual(
        name = "Category",
        values = plot_colors
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        title = "Category"
      )) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$Region)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(title = title)

    return(fig)
  } else {
    print("NO DATA")
  }
}
