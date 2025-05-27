test_that("read_rec_catch reads and cleans correctly", {
  temp_dir <- tempdir()
  species <- "fake species"
  formatted_species <- stringr::str_to_upper(species) |>
    stringr::str_replace_all(" ", "_")
  temp_file <- file.path(temp_dir, paste0(formatted_species, "_TEST.csv"))
  
  # Create 46 empty lines, then a header line
  lines <- c(rep("", 46), "YEAR,STATE,TOTAL_CATCH_A_B1_B2")
  writeLines(lines, temp_file)
  write.table(
    data.frame(YEAR = 2020, STATE = "NY", TOTAL_CATCH_A_B1_B2 = 123),
    file = temp_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE
  )
  
  result <- read_rec_catch(species, dir = temp_dir)
  
  expect_s3_class(result, "data.frame")
  expect_true("TOTAL_CATCH_A_B1_B2" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("read_rec_catch errors when no matching file is found", {
  temp_dir <- tempdir()
  # Choose a species name that will not match any file in temp_dir
  species <- "nonexistent species"
  
  expect_error(
    read_rec_catch(species, dir = temp_dir),
    regexp = "No file found for species"
  )
})
