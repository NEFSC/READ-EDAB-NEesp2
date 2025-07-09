test_that("create_total_rec_landings summarizes correctly", {
  data <- tibble::tibble(
    YEAR = 2020,
    STATE = "NEW YORK",  # Must match default states in function
    HARVEST_A_B1_TOTAL_WEIGHT_LB = 123.4
  )
  
  result <- create_total_rec_landings(data, states = "NEW YORK", groupby_state = FALSE)
  
  expect_equal(result$DATA_VALUE, 123.4)
  expect_equal(result$INDICATOR_UNITS, "lbs")
  expect_equal(result$INDICATOR_NAME, "total_recreational_landings_lbs")
  expect_equal(result$CATEGORY, "Recreational")
  expect_equal(result$INDICATOR_TYPE, "Socioeconomic")
  expect_equal(result$YEAR, 2020)
})

