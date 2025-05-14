
#' Scrape MRIP catch data from MRIP Query tool
#' 
#' This function scrapes MRIP catch data from the MRIP Query tool
#' 
#' @param species the common name of the species as it appears in the MRIP data. capitalization does not matter.
#' @return Returns a list of the scraped data and metadata.
#' @export

get_mrip_catch <- function(species) {
  
  species <- species |> 
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "%20")
  
    url <- paste0("https://www.st.nmfs.noaa.gov/SASStoredProcess/guest?_program=%2F%2FFoundation%2FSTP%2Fmrip_series_catch&qyearfrom=1981&qyearto=2024&qsummary=cumulative_pyc&qwave=1&fshyr=annual&qstate=NORTH+AND+MID-ATLANTIC&qspecies=", 
                  species, 
                  "&qmode_fx=ALL+MODES+COMBINED&qarea_x=ALL+AREAS+COMBINED&qcatch_type=ALL+CATCH+TYPES+%28TYPE+A%2C+B1%2C+and+B2%29&qdata_type=NUMBERS+OF+FISH&qoutput_type=TABLE&qsource=PRODUCTION")
 
  test <- httr::GET(url) |>
    httr::content()
  
  tbl <- test |>
    xml2::xml_child(2) |>
    xml2::xml_child(1)
  
  meta_tbl <- xml2::xml_child(tbl, 2)
  data_tbl <- xml2::xml_child(tbl, 4)
  
  tbl2 <- rvest::html_table(data_tbl)
  meta2 <- rvest::html_text(meta_tbl)
  
  output <- list(data = tbl2,
                 metadata = meta2)
  
  return(output)
  
}

# bsb_test <- get_mrip_catch("BLACK SEA BASS")

#' Scrape MRIP trip data from MRIP Query tool
#' 
#' This function scrapes MRIP trip data from the MRIP Query tool
#' 
#' @param species the common name of the species as it appears in the MRIP data. capitalization does not matter.
#' @param region the name of the region. Can be a state name, "North Atlantic", "Mid-Atlantic", etc. Capitalization does not matter. 
#' @param year the year of data to query. Must be a single value. The earliest year possible is 1981.
#' @return Returns a list of the scraped data and metadata.
#' @export

get_mrip_trips <- function(species,
                           region,
                           year) {
  species <- species |> 
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "%20")
  
  region <- region |> 
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "+")
  
url <- paste0("https://www.st.nmfs.noaa.gov/SASStoredProcess/guest?_program=%2F%2FFoundation%2FSTP%2Fmrip_directed_trip&qyearfrom=",
              year,
              "&qsummary=cumulative_pya&qwave=1&fshyr=annual&qstate=",
              region,
              "&qspecies=",
              species,
              "&qmode_fx=ALL+MODES+COMBINED&qarea_x=ALL+AREAS+COMBINED&qsp_opt=PRIMARY&qsp_opt=SECONDARY&qsp_opt=CAUGHT&qsp_opt=HARVESTED&qsp_opt=RELEASED&qoutput_type=TABLE&qsource=PRODUCTION")


test <- httr::GET(url) |>
  httr::content()

tbl <- test |>
  xml2::xml_child(2) |>
  xml2::xml_child(2)

meta_tbl <- xml2::xml_child(tbl, 2)
data_tbl <- tbl |>
  xml2::xml_child(4) |>
  xml2::xml_child(1) |>
  xml2::xml_child(1)

tbl2 <- rvest::html_table(data_tbl) |>
  dplyr::mutate(Species = species,
                Region = region)
meta2 <- rvest::html_text(meta_tbl)

output <- list(data = tbl2,
               metadata = meta2)

return(output)
}

# get_mrip_trips(species = "black sea bass",
#                year = 2024,
#                region = "north atlantic")
# 
# species <- c("Atlantic cod",
#               "Atlantic mackerel",
#               "Black sea bass",
#               "Chub mackerel",
#               "Haddock",
#               "Pollock",
#               "Scup",
#               "Striped bass",
#               "Summer flounder",
#               "Winter flounder")
# 
# params <- expand.grid(region = c("north atlantic", "mid-atlantic"), 
#                       year = 1981:1982) #,
#                       # species = species)
# 
# purrr::map(purrr::list_transpose(list(region = params$region, 
#                                       year = params$year)),
#             ~save_trips(this_species = "black sea bass",
#                         this_year = .x$year,
#                         this_region = .x$region,
#                         out_folder = here::here("data-raw/test")))
# 
# get_mrip_trips(species = "black sea bass",
#                year = 1981,
#                region = "north atlantic")

#' Scrape and save MRIP trip data from MRIP Query tool
#' 
#' This function scrapes MRIP trip data from the MRIP Query tool and saves it as an Rds. 
#' Used as a helper function to automate data pulls.
#' 
#' @param this_species the common name of the species as it appears in the MRIP data. capitalization does not matter.
#' @param this_region the name of the region. Can be a state name, "North Atlantic", "Mid-Atlantic", etc. Capitalization does not matter. 
#' @param this_year the year of data to query. Must be a single value. The earliest year possible is 1981.
#' @param out_folder where to save the data
#' @return Returns a list of the scraped data and metadata.
#' @export

save_trips <- function(this_species, this_year, this_region, out_folder) {
  fname <- paste0(out_folder, 
                  paste("/trips", this_species, this_region, this_year, sep = "_"),
                ".Rds") |>
    stringr::str_replace_all(" ", "_")
  
  out <- get_mrip_trips(species = this_species,
                        year = this_year,
                        region = this_region)
  
  saveRDS(out, fname)
}

#' Scrape and save MRIP catch data from MRIP Query tool
#' 
#' This function scrapes MRIP catch data from the MRIP Query tool and saves it as an Rds. 
#' Used as a helper function to automate data pulls.
#' 
#' @param this_species the common name of the species as it appears in the MRIP data. capitalization does not matter.
#' @param out_folder where to save the data
#' @return Returns a list of the scraped data and metadata.
#' @export

save_catch <- function(this_species, out_folder) {
  fname <- paste0(out_folder, 
                  "/catch_",
                  this_species,
                  ".Rds") |>
    stringr::str_replace_all(" ", "_")
  
  out <- get_mrip_catch(species = this_species)
  
  saveRDS(out, fname)
}

# save_catch(this_species = "black sea bass", 
#            out_folder = here::here("data-raw/test"))
# get_mrip_catch("black sea bass")
