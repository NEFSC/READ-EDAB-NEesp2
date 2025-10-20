# function to create rec hms indicator

pull_rec_hms <- function(outputDir) {
  ## set up query ----
  species_list <- c(
    'atlantic angel shark',
    'atlantic sharpnose shark',
    'basking shark',
    'bigeye thresher',
    'bignose shark',
    'blacknose shark',
    'blacktip shark',
    'blue shark',
    'bonnethead',
    'bull shark',
    'dusky shark',
    'finetooth shark',
    'great hammerhead',
    'lemon shark',
    'night shark',
    'nurse shark',
    'oceanic whitetip',
    'oceanic whitetip shark',
    'porbeagle',
    'sand tiger',
    'sandbar shark',
    'scalloped hammerhead shark',
    'sevengill shark',
    'shortfin mako',
    'silky shark',
    'sixgill shark',
    'smooth hammerhead',
    'spinner shark',
    'thresher shark',
    'tiger shark',
    'white shark',
    # billfishes
    'billfish family',
    'black marlin',
    'blue marlin',
    'shortbill spearfish',
    'striped marlin',
    # tunas -- not including mackerels and pacific fish
    'albacore',
    'bigeye tuna',
    'bluefin tuna',
    'skipjack tuna',
    'striped bonito',
    'wahoo',
    'yellowfin tuna'
  )

  region_list <- c('north atlantic', 'mid-atlantic')

  query_params <- expand.grid(species = species_list, regions = region_list)

  ## query data ----
  data_pull <- purrr::map2(
    query_params$species,
    query_params$regions,
    ~ {
      save_data <- NEesp2::save_catch(
        this_species = .x,
        this_region = .y,
        this_data_type = "numbers of fish",
        out_folder = paste0(outputDir, "/hms_mrip_data"),
        catch_type = "landings",
        wait = FALSE
      )

      data <- readRDS(save_data)

      if (is.data.frame(data$data)) {
        create_total_mrip(
          data$data,
          var_name = "hms_landings",
          var_units = "n",
          remove_non_standard = FALSE
        )
      }
    }
  )

  ## bind data together and save intermediate ----
  new_hms <- purrr::reduce(data_pull, dplyr::bind_rows)

  write.csv(
    new_hms,
    paste0(outputDir, "hms_mrip_", Sys.Date(), ".csv"),
    row.names = FALSE
  )

  return(new_hms)
}

create_rec_hms <- function(
  inputFile,
  key = NEesp2::hms_key # this could be hardcoded into the function, rather than passed as a parameter
) {
  # data wrangling ----
  rec_hms <- read.csv(inputFile) |>
    dplyr::left_join(
      key |> dplyr::select(COMMON_NAME, SP_CATEGORY),
      by = c("SPECIES" = "COMMON_NAME")
    ) |>
    dplyr::group_by(YEAR, SP_CATEGORY, REGION) |>
    dplyr::summarise(
      Value = sum(DATA_VALUE)
    ) |>
    dplyr::rename(
      Var = SP_CATEGORY,
      Time = YEAR,
      EPU = REGION
    ) |>
    dplyr::mutate(
      EPU = dplyr::case_when(
        EPU == "MID-ATLANTIC" ~ "MAB",
        EPU == "NORTH ATLANTIC" ~ "NE"
      )
    ) |>
    dplyr::mutate(
      Var = paste0(Var, "-", EPU)
    ) |>
    dplyr::select(Time, Var, Value, EPU) |>
    dplyr::filter(!stringr::str_detect(Var, "Pelagic")) # Remove pelagics from this dataset per HMS request

  return(rec_hms)
}

# test <- create_rec_hms(
#   inputFile = here::here("data-raw/hms_mrip/2025-08-26/hms_mrip_2025-08-26.csv")
# )

# test |>
#   dplyr::mutate(
#     group = stringr::str_detect(Var, "Scombridae"),
#     Var = stringr::str_remove(Var, "-.+")
#   ) |>
#   ggplot2::ggplot(ggplot2::aes(x = Time, y = Value / 1000, color = Var)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_line() +
#   ggplot2::theme_bw() +
#   ggplot2::facet_grid(
#     cols = ggplot2::vars(EPU),
#     rows = ggplot2::vars(group),
#     scales = "free_y"
#   ) +
#   ggplot2::ylab("Thousands of fish") +
#   ggplot2::theme(strip.text.y = ggplot2::element_blank())
