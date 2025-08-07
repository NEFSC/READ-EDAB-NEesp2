###############################################

devtools::load_all()

species_list <- c(
  'atlantic angel shark',
  'atlantic sharpnose shark',
  'basking shark',
  'bigeye thresher',
  'blacknose shark',
  'blacktip shark',
  'blue shark',
  'bonnethead, sixgill shark, sevengill shark, bignose shark, night shark',
  'bull shark',
  'dusky shark',
  'finetooth shark',
  'great hammerhead',
  'lemon shark',
  'nurse shark',
  'oceanic whitetip',
  'oceanic whitetip shark',
  'porbeagle',
  'sand tiger',
  'sandbar shark',
  'scalloped hammerhead shark',
  'shortfin mako',
  'silky shark',
  'smooth hammerhead',
  'spinner shark',
  'thresher shark',
  'tiger shark',
  'white shark'
)

region_list <- c('north atlantic', 'mid-atlantic')

query_params <- expand.grid(species = species_list, regions = region_list)

data_pull <- purrr::map2(
  query_params$species[1:5],
  query_params$regions[1:5],
  ~ {
    save_data <- save_catch(
      this_species = .x,
      this_region = .y,
      out_folder = here::here("data-raw/hms_mrip/2025-08-04"),
      catch_type = "landings",
      wait = FALSE
    )

    data <- readRDS(save_data)

    if (is.data.frame(data$data)) {
      create_total_mrip(data$data, remove_non_standard = FALSE)
    }
  }
)

output <- purrr::reduce(data_pull, dplyr::bind_rows)


## Runs MRIP pull for all species in 'species_list', saves Rds files and csv for each species
purrr::map(
  species_list,
  ~ {
    save_data <- save_catch(
      this_species = .x,
      out_folder = here::here('data-raw/hms_mrip/'),
      catch_type = "landings"
    )

    data <- readRDS(save_data)

    hms_catch <- create_total_rec_catch(data$data, remove_non_standard = FALSE)

    write.csv(hms_catch, here::here(paste0("hms_catch_", .x, ".csv")))
  }
)

## Read in csv files
hms_files <- list.files(
  path = here::here('data-raw/hms_mrip'),
  pattern = "*.csv",
  full.names = TRUE
)
hms_files
hms_mrip <- hms_files |>
  purrr::map(~ read.csv(.x))

## Convert to df and combine all files, format column names for SOE
hms_mrip_combined <- purrr::map_df(hms_mrip, .f = identity) |>
  subset(select = -c(1, 4:5)) |>
  dplyr::rename(
    Time = YEAR,
    Var = INDICATOR_NAME,
    Value = DATA_VALUE,
    Units = INDICATOR_UNITS,
    Species = SPECIES
  )

write.csv(hms_mrip_combined, here::here('data-raw/hms_mrip_combined.csv'))

##################################################################

###Test (doesn't work: bonnethead, sixgill shark, sevengill shark, bignose shark, night shark)

smooth_hammerhead <- save_catch(
  this_species = "smooth hammerhead",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)

sh_rds <- readRDS(smooth_hammerhead)
sh_rds
sh_mrip <- create_total_rec_catch(sh_rds$data, remove_non_standard = FALSE)


scalloped_hammerhead <- save_catch(
  this_species = "scalloped hammerhead shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
sch_rds <- readRDS(scalloped_hammerhead)
sch_rds
sch_mrip <- create_total_rec_catch(sch_rds$data, remove_non_standard = FALSE)


great_hammerhead <- save_catch(
  this_species = "great hammerhead",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
gh_rds <- readRDS(here::here(
  'data-raw/hms_mrip/catch_landings_great_hammerhead.Rds'
))
gh_rds
gh_mrip <- create_total_rec_catch(gh_rds$data, remove_non_standard = FALSE)


atlantic_angel_shark <- save_catch(
  this_species = "atlantic angel shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
aas_rds <- readRDS(atlantic_angel_shark)
aas_rds
aas_mrip <- create_total_rec_catch(aas_rds$data, remove_non_standard = FALSE)


nurse_shark <- save_catch(
  this_species = "nurse shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
ns_rds <- readRDS(nurse_shark)
ns_rds
ns_mrip <- create_total_rec_catch(ns_rds$data, remove_non_standard = FALSE)


sand_tiger <- save_catch(
  this_species = "sand tiger",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
sand_rds <- readRDS(sand_tiger)
sand_rds
sand_mrip <- create_total_rec_catch(sand_rds$data, remove_non_standard = FALSE)


white_shark <- save_catch(
  this_species = "white shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
white_rds <- readRDS(white_shark)
white_rds
white_mrip <- create_total_rec_catch(
  white_rds$data,
  remove_non_standard = FALSE
)


basking_shark <- save_catch(
  this_species = "basking shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
basking_rds <- readRDS(basking_shark)
basking_rds
basking_mrip <- create_total_rec_catch(
  basking_rds$data,
  remove_non_standard = FALSE
)


porbeagle <- save_catch(
  this_species = "porbeagle",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
porbeagle_rds <- readRDS(porbeagle)
porbeagle_rds
porbeagle_mrip <- create_total_rec_catch(
  porbeagle_rds$data,
  remove_non_standard = FALSE
)


thresher_shark <- save_catch(
  this_species = "thresher shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
thresher_rds <- readRDS(thresher_shark)
thresher_rds
thresher_mrip <- create_total_rec_catch(
  thresher_rds$data,
  remove_non_standard = FALSE
)


bigeye <- save_catch(
  this_species = "bigeye thresher",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
bigeye_rds <- readRDS(bigeye)
bigeye_rds
bigeye_mrip <- create_total_rec_catch(
  bigeye_rds$data,
  remove_non_standard = FALSE
)


mako <- save_catch(
  this_species = "shortfin mako",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
mako_rds <- readRDS(mako)
mako_rds
mako_mrip <- create_total_rec_catch(mako_rds$data, remove_non_standard = FALSE)


tiger_shark <- save_catch(
  this_species = "tiger shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
tiger_rds <- readRDS(tiger_shark)
tiger_rds
tiger_mrip <- create_total_rec_catch(
  tiger_rds$data,
  remove_non_standard = FALSE
)


sharpnose <- save_catch(
  this_species = "atlantic sharpnose shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
sharpnose_rds <- readRDS(sharpnose)
sharpnose_rds
sharpnose_mrip <- create_total_rec_catch(
  sharpnose_rds$data,
  remove_non_standard = FALSE
)


dusky_shark <- save_catch(
  this_species = "dusky shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
dusky_rds <- readRDS(dusky_shark)
dusky_rds
dusky_mrip <- create_total_rec_catch(
  dusky_rds$data,
  remove_non_standard = FALSE
)


bull <- save_catch(
  this_species = "bull shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
bull_rds <- readRDS(bull)
bull_rds
bull_mrip <- create_total_rec_catch(bull_rds$data, remove_non_standard = FALSE)


sandbar <- save_catch(
  this_species = "sandbar shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
sandbar_rds <- readRDS(sandbar)
sandbar_rds
sandbar_mrip <- create_total_rec_catch(
  sandbar_rds$data,
  remove_non_standard = FALSE
)


blacknose <- save_catch(
  this_species = "blacknose shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
blacknose_rds <- readRDS(blacknose)
blacknose_rds
blacknose_mrip <- create_total_rec_catch(
  blacknose_rds$data,
  remove_non_standard = FALSE
)


silky <- save_catch(
  this_species = "silky shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
silky_rds <- readRDS(silky)
silky_rds
silky_mrip <- create_total_rec_catch(
  silky_rds$data,
  remove_non_standard = FALSE
)


blacktip <- save_catch(
  this_species = "blacktip shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
blacktip_rds <- readRDS(blacktip)
blacktip_rds
blacktip_mrip <- create_total_rec_catch(
  blacktip_rds$data,
  remove_non_standard = FALSE
)


whitetip <- save_catch(
  this_species = "oceanic whitetip shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
whitetip_rds <- readRDS(whitetip)
whitetip_rds
whitetip_mrip <- create_total_rec_catch(
  whitetip_rds$data,
  remove_non_standard = FALSE
)


spinner <- save_catch(
  this_species = "spinner shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
spinner_rds <- readRDS(spinner)
spinner_rds
spinner_mrip <- create_total_rec_catch(
  spinner_rds$data,
  remove_non_standard = FALSE
)


blue <- save_catch(
  this_species = "blue shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
blue_rds <- readRDS(blue)
blue_rds
blue_mrip <- create_total_rec_catch(blue_rds$data, remove_non_standard = FALSE)


lemon <- save_catch(
  this_species = "lemon shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
lemon_rds <- readRDS(lemon)
lemon_rds
lemon_mrip <- create_total_rec_catch(
  lemon_rds$data,
  remove_non_standard = FALSE
)


finetooth <- save_catch(
  this_species = "finetooth shark",
  out_folder = here::here("data-raw/hms_mrip"),
  catch_type = "landings"
)
finetooth_rds <- readRDS(finetooth)
finetooth_rds
finetooth_mrip <- create_total_rec_catch(
  finetooth_rds$data,
  remove_non_standard = FALSE
)
