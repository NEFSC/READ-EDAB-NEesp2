
#doesn't work yet, error in readRDS = bad 'file' argument


hms_mrip <- function(species_list) {
  ## Runs MRIP pull for all species in 'species_list', saves Rds files and csv for each species  
  purrr::map(
    species_list,
    ~ {
      save_data <- NEesp2::save_catch(
        this_species = .x,
        out_folder = here::here('data-raw/hms_mrip/'),
        catch_type = "landings"
      )
      
      data <- readRDS(save_data)
      
      hms_catch <- NEesp2::create_total_rec_catch(data$data, remove_non_standard = FALSE)
      
      write.csv(hms_catch, here::here(paste0("hms_catch_", .x, ".csv")))
    }
  )
  
  ## Read in csv files 
  hms_files <- list.files(path = here::here('data-raw/hms_mrip'), pattern = "*.csv", full.names = TRUE)
  hms_mrip <- hms_files |> 
    purrr::map(~ read.csv(.x))
  
  ## Convert to df and combine all files, format column names for SOE
  hms_mrip_combined <- purrr::map_df(hms_mrip, .f = identity) |>
    subset(select = -c(1,4:5)) |>
    dplyr::rename(Time = YEAR,
                  Var = INDICATOR_NAME,
                  Value = DATA_VALUE,
                  Units = INDICATOR_UNITS,
                  Species = SPECIES)
  
  return(hms_mrip_combined)
}

test <- hms_mrip(species_list = c("smooth hammerhead", "scalloped hammerhead shark", "great hammerhead",
                          "atlantic angel shark", "nurse shark", "sand tiger", "white shark",
                          "basking shark", "porbeagle", "thresher shark", "bigeye thresher",
                          "shortfin mako", "tiger shark", "atlantic sharpnose shark",
                          "dusky shark", "bull shark", "sandbar shark", "blacknose shark",
                          "silky shark", "blacktip shark", "oceanic whitetip shark",
                          "spinner shark", "blue shark", "lemon shark", "finetooth shark"))
