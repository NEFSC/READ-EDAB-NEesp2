###############################################

devtools::load_all()

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
  'white shark'
)

region_list <- c('north atlantic', 'mid-atlantic')

query_params <- expand.grid(species = species_list, regions = region_list)

data_pull <- purrr::map2(
  query_params$species,
  query_params$regions,
  ~ {
    save_data <- save_catch(
      this_species = .x,
      this_region = .y,
      this_data_type = "numbers of fish",
      out_folder = here::here("data-raw/hms_mrip/2025-08-04"),
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

output <- purrr::reduce(data_pull, dplyr::bind_rows)
write.csv(
  output,
  here::here("data-raw/hms_mrip/hms_mrip_2025-08-04.csv"),
  row.names = FALSE
)
