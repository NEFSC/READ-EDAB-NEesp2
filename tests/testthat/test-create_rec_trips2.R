# test_that("create_mrip_trips processes Rds files correctly", {
#   temp_dir <- tempdir()
#   species <- "Fake Fish"
#   species_dir <- file.path(temp_dir, "FAKE_FISH")
#   dir.create(species_dir)
#
#   data <- list(
#     data = tibble::tibble(
#       YEAR = 2020,
#       DIRECTED_TRIPS = "1,000",
#       REGION = "Region 1",
#       SPECIES = species,
#       DOES_DIRECTED_TRIPS_MEET_MRIP_STANDARD = "Yes"
#     )
#   )
#
#   saveRDS(data, file.path(species_dir, "2020.Rds"))
#
#   result <- create_mrip_trips(temp_dir, species)
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("YEAR", "DATA_VALUE") %in% names(result)))
# })
