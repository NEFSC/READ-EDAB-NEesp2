test_that("create_total_rec_catch filters correctly", {
  mock_data <- tibble::tibble(
    YEAR = 2020,
    TOTAL_CATCH_A_B1_B2 = 100,
    DOES_TOTAL_CATCH_A_B1_B2_MEET_MRIP_STANDARD = "YES"
  )
  
  result <- create_total_rec_catch(mock_data, species = "cod")
  
  expect_equal(nrow(result), 1)
  expect_equal(result$DATA_VALUE, 100)
})

test_that("create_total_rec_catch filters out non-standard data", {
  mock_data <- tibble::tibble(
    YEAR = 2021,
    TOTAL_CATCH_A_B1_B2 = 200,
    DOES_TOTAL_CATCH_A_B1_B2_MEET_MRIP_STANDARD = "NO"
  )
  
  result <- create_total_rec_catch(mock_data, species = "haddock")
  
  expect_equal(nrow(result), 0)
})
