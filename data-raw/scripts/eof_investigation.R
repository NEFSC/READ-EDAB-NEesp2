gb_area <- ecodata::epu_sf |>
  dplyr::filter(EPU == "GB") |>
  sf::st_area() /
  1e6 # convert to km2

ecodata::ppr |>
  dplyr::filter(EPU == "GB", Var == "Ryther" | Var == "PP") |>
  dplyr::select(-Units) |>
  tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  dplyr::mutate(
    total_energy_required = (Ryther * gb_area) |> units::drop_units(),
    ratio = total_energy_required / PP
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = ratio)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()
