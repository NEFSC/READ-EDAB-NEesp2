test_that("create_prop_sp_trips computes proportions", {
  total <- tibble::tibble(
    YEAR = 2020,
    STATE = "NEW YORK",  # Must match states list in function
    ANGLER_TRIPS = 2000  # Use numeric type
  )
  
  species <- tibble::tibble(
    YEAR = 2020,
    STATE = "NEW YORK",
    DATA_VALUE = 500
  )
  
  result <- create_prop_sp_trips(
    total_trips = total,
    species_trips = species,
    states = "NEW YORK",  # explicitly set
    groupby_state = TRUE
  )
  
  expect_true("DATA_VALUE" %in% names(result))
  expect_equal(result$DATA_VALUE, 500 / 2000, tolerance = 1e-8)
})

test_that("create_prop_sp_trips returns no rows if state not in filter", {
  total <- tibble::tibble(YEAR = 2020, STATE = "FLORIDA", ANGLER_TRIPS = 1000)
  species <- tibble::tibble(YEAR = 2020, STATE = "FLORIDA", DATA_VALUE = 100)
  
  result <- create_prop_sp_trips(total, species, states = "NEW YORK", groupby_state = TRUE)
  expect_equal(nrow(result), 0)
})

test_that("create_prop_sp_trips works without state grouping", {
  total <- tibble::tibble(YEAR = 2020, STATE = "NEW YORK", ANGLER_TRIPS = 1000)
  species <- tibble::tibble(YEAR = 2020, STATE = "NEW YORK", DATA_VALUE = 250)
  
  result <- create_prop_sp_trips(total, species, states = "NEW YORK", groupby_state = FALSE)
  expect_equal(result$DATA_VALUE, 250 / 1000, tolerance = 1e-8)
})