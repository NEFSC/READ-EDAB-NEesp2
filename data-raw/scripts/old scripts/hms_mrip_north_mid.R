

# HMS Pulls from MRIP
# As of 7/31/25, north and mid atlantic need to be run separately as there are species in mid-atlantic that do not have output in north (and vice versa
# The following species do not return any data for either North or Mid:
      #bonnethead, sixgill shark, sevengill shark, bignose shark, night shark

####################################################
#### NORTH ATLANTIC ####
#removed from north atlantic: great hammerhead, basking shark, atlantic sharpnose shark, bull shark, silky shark
#oceanic whitetip, blacknose shark, lemon shark, finetooth shark

species_list <- c("smooth hammerhead", "scalloped hammerhead shark",
                  "atlantic angel shark", "nurse shark", "sand tiger", "white shark",
                  "porbeagle", "thresher shark", "bigeye thresher",
                  "shortfin mako", "tiger shark", "dusky shark", 
                  "sandbar shark", "blacktip shark", 
                  "spinner shark", "blue shark")

region_list <- c('north atlantic')

max_length <- max(length(species_list), length(region_list))

species_recycled <- rep(species_list, length.out = max_length)
region_recycled <- rep(region_list, length.out = max_length)

purrr::map2(
  species_recycled,
  region_recycled,
  ~ {
    save_data <- save_catch(
      this_species = .x,
      this_region = .y,
      out_folder = here::here('data-raw/hms_mrip/north_atlantic/'),
      catch_type = "all"
    )
    
    data <- readRDS(save_data)
    
    esp_catch <- create_total_mrip(data$data, remove_non_standard = FALSE)
    
    write.csv(esp_catch, here::here(paste0("data-raw/hms_mrip/north_atlantic/north_atlantic_", .x, ".csv")))
  }
)

## Read in csv files 
na_files <- list.files(path = here::here('data-raw/hms_mrip/north_atlantic'), pattern = "*.csv", full.names = TRUE)
na_mrip <- na_files |> 
  purrr::map(~ read.csv(.x))

na_mrip_combined <- purrr::map_df(na_mrip, .f = identity) |>
  subset(select = -c(1,4:5)) |>
  dplyr::rename(Time = YEAR,
                Var = INDICATOR_NAME,
                Value = DATA_VALUE,
                Units = INDICATOR_UNITS,
                Species = SPECIES)

write.csv(na_mrip_combined, here::here('data-raw/north_atlantic_hms_mrip.csv'))

###############################################
#### MID ATLANTIC ####

#removed: bigeye thresher

species_list <- c("smooth hammerhead", "scalloped hammerhead shark", "great hammerhead",
                  "atlantic angel shark", "nurse shark", "sand tiger", "white shark",
                  "basking shark", "porbeagle", "thresher shark", 
                  "shortfin mako", "tiger shark", "atlantic sharpnose shark",
                  "dusky shark", "bull shark", "sandbar shark", "blacknose shark",
                  "silky shark", "blacktip shark", "oceanic whitetip shark",
                  "spinner shark", "blue shark", "lemon shark", "finetooth shark")

region_list <- c('mid-atlantic')

max_length <- max(length(species_list), length(region_list))

species_recycled <- rep(species_list, length.out = max_length)
region_recycled <- rep(region_list, length.out = max_length)

purrr::map2(
  species_recycled,
  region_recycled,
  ~ {
    save_data <- save_catch(
      this_species = .x,
      this_region = .y,
      out_folder = here::here('data-raw/hms_mrip/mid_atlantic/'),
      catch_type = "all"
    )
    
    data <- readRDS(save_data)
    
    esp_catch <- create_total_mrip(data$data, remove_non_standard = FALSE)
    
    write.csv(esp_catch, here::here(paste0("data-raw/hms_mrip/mid_atlantic/mid_atlantic_", .x, ".csv")))
  }
)

## Read in csv files
mid_files <- list.files(path = here::here('data-raw/hms_mrip/mid_atlantic'), pattern = "*.csv", full.names = TRUE)
mid_mrip <- mid_files |> 
  purrr::map(~ read.csv(.x))

mid_mrip_combined <- purrr::map_df(mid_mrip, .f = identity) |>
  subset(select = -c(1,4:5)) |>
  dplyr::rename(Time = YEAR,
                Var = INDICATOR_NAME,
                Value = DATA_VALUE,
                Units = INDICATOR_UNITS,
                Species = SPECIES)

write.csv(mid_mrip_combined, here::here('data-raw/mid_atlantic_hms_mrip.csv'))