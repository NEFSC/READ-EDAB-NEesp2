devtools::load_all()

condition <- NEesp2::species_condition(
  data = NEesp2::survdat_subset,
  LWparams = NEesp2::LWparams,
  species.codes = NEesp2::species.codes,
  output = "soe"
)

head(condition)
head(ecodata::condition)

ecodata::condition |>
  dplyr::filter(Var == "Black sea bass") |>
  dplyr::mutate(source = "ecodata") |>
  dplyr::bind_rows(
    condition |>
      dplyr::mutate(source = "new")
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = Time,
    y = Value,
    color = EPU,
    shape = source
  )) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Black sea bass condition comparison")
