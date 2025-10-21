
#' Read in MRIP recreational catch data
#'
#' This function read in MRIP catch data
#'
#' @param species the species common name
#' @param type the type of data to read in, options are "all", "private", "charter", or "for_hire"
#' @param dir the directory where MRIP catch files are saved
#' @return a tibble
#' @export

## TODO: not sure this function is needed anymore

read_rec_catch <- function(species, dir, type = "all") {
  new_species <- species |>
    stringr::str_to_upper() |>
    stringr::str_replace_all(" ", "_")
  
  files <- list.files(
    path = dir,
    full.names = TRUE
  )
  
  this_file <- files[which(stringr::str_detect(stringr::str_to_upper(files),
                                               pattern = paste0(
                                                 stringr::str_to_upper(type),
                                                 "_",
                                                 stringr::str_to_upper(species)
                                               ) |>
                                                 stringr::str_replace_all(" ", "_")
  ))]
  
  # read in the data
  rec_catch <- readRDS(this_file) |>
    purrr::map(~ janitor::clean_names(.x[1],
                                      case = "all_caps"
    ))
  
  return(rec_catch)
}

#'
#' This function returns a list of MRIP trip files
#' @param dir A directory that has subfolders with species-level data
#' @param species the species of interest
#' @return A vector of files
#' @export

get_trip_files <- function(dir, species) {
  new_dir <- list.dirs(dir,
                       full.names = TRUE
  )
  this_dir <- new_dir[which(stringr::str_detect(stringr::str_to_upper(new_dir),
                                                pattern = stringr::str_to_upper(species) |>
                                                  stringr::str_replace_all(" ", "_")
  ))]
  
  files <- list.files(this_dir,
                      pattern = "[0-9].Rds",
                      full.names = TRUE
  )
  
  return(files)
}