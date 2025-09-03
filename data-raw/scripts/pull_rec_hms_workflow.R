#' Pulls recreational HMS data from MRIP
#'
#' This pulls landings data for HMS species in the North and Mid-Atlantic. 
#' The function creates an rds and csv file for each species, to be stored in the designated output directory.
#' The function then combines all individual output files and saves as 'hms_mrip_' followed by the date in the output directory. 
#'
#' @param outputDir Character string. Full path the directory in which to store files.
#' 
#' @return new.hms, a csv file with landings data from MRIP
#'
#' @export


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