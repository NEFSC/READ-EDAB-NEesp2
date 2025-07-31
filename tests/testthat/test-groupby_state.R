test_that("groupby_state groups correctly by YEAR only", {
  df <- tibble::tibble(YEAR = c(2020, 2020, 2021),
                       STATE = c("NY", "MA", "NY"),
                       VALUE = c(1, 2, 3))
  
  grouped <- groupby_state(df, groupby = FALSE)
  expect_true("YEAR" %in% dplyr::group_vars(grouped))
  expect_false("STATE" %in% dplyr::group_vars(grouped))
})

test_that("groupby_state groups correctly by YEAR and STATE", {
  df <- tibble::tibble(YEAR = c(2020, 2020, 2021),
                       STATE = c("NY", "MA", "NY"),
                       VALUE = c(1, 2, 3))
  
  grouped <- groupby_state(df, groupby = TRUE)
  expect_setequal(dplyr::group_vars(grouped), c("YEAR", "STATE"))
})
