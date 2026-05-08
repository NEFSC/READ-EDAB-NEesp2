gb_area <- ecodata::epu_sf |>
  dplyr::filter(EPU == "GB") |>
  sf::st_area() /
  1e6 # convert to km2

new_ppr <- ecodata::ppr |>
  dplyr::filter(EPU == "GB", Var == "Ryther" | Var == "PP") |>
  dplyr::select(-Units) |>
  tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  dplyr::mutate(
    total_fish = (Ryther * gb_area) |> units::drop_units(),
    ppr = (total_fish / 9) * (1 / 0.15)^2.5,
    ratio = ppr / PP
  )

new_ppr |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = ratio)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

# compare to pp from annual_chl_pp

ecodata::plot_annual_chl_pp(
  report = "NewEngland",
  EPU = "GB",
  varName = "pp",
  plottype = "total"
)

fix <- dplyr::filter(
  dplyr::mutate(
    ecodata::annual_chl_pp,
    Time = as.integer(gsub("A_", "", Time))
  ),
  EPU == "GB",
  Var == "PPD_ANNUAL_MTON"
)

ppr <- ecodata::ppr |>
  dplyr::filter(EPU == "GB", Var == "PP")

combined_pp <- tibble::tibble(
  Time = c(
    fix$Time,
    ppr |>
      dplyr::pull(Time)
  ),
  pp = c(
    fix$Value,
    ppr |>
      dplyr::pull(Value)
  ),
  source = c(
    rep("annual_chl_pp", nrow(fix)),
    rep(
      "ppr",
      length(
        ppr |>
          dplyr::pull(Time)
      )
    )
  )
)

combined_pp |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = pp, color = source)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

# compare catch in ppr to ecodata::comdat

ecodata::comdat |>
  dplyr::filter(Var == "Landings", EPU == "GB") |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::geom_point(
    data = new_ppr,
    ggplot2::aes(y = total_fish),
    color = "red"
  )

reconstructed_ppr <- dplyr::full_join(
  ecodata::comdat |>
    dplyr::filter(Var == "Landings", EPU == "GB") |>
    dplyr::select(Time, Value) |>
    dplyr::mutate(Var = "Landings"),
  ecodata::annual_chl_pp |>
    dplyr::filter(EPU == "GB", Var == "PPD_ANNUAL_MTON") |>
    dplyr::select(Time, Value) |>
    dplyr::mutate(
      Var = "PPD_ANNUAL_MTON",
      Time = as.integer(gsub("A_", "", Time))
    )
) |>
  tidyr::pivot_wider(names_from = Var, values_from = Value) |>
  dplyr::mutate(
    ppr = (Landings / 9) * (1 / 0.15)^2.5,
    # fill in missing pp
    PPD_ANNUAL_MTON = ifelse(
      is.na(PPD_ANNUAL_MTON),
      mean(PPD_ANNUAL_MTON, na.rm = TRUE),
      PPD_ANNUAL_MTON
    ),
    ratio = ppr / PPD_ANNUAL_MTON
  )

reconstructed_ppr |>
  ggplot2::ggplot(ggplot2::aes(x = Time, y = ratio)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()
