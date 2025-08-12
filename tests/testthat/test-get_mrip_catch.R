library(testthat)
library(mockery)
library(rvest)
library(dplyr)

# Load the function from your file
source(here::here("R/get_mrip_data.R"))

# Create mock HTML content for the different scenarios
# mock_html_with_data <- httr::GET(
#   "https://www.st.nmfs.noaa.gov/SASStoredProcess/guest?_program=%2F%2FFoundation%2FSTP%2Fmrip_series_catch&qyearfrom=1981&qyearto=2024&qsummary=cumulative_pya&qwave=1&fshyr=annual&qstate=NORTH+AND+MID-ATLANTIC&qspecies=BLACK%20SEA%20BASS&qmode_fx=ALL+MODES+COMBINED&qarea_x=ALL+AREAS+COMBINED&qcatch_type=TOTAL+CATCH+%28TYPE+A+%2B+B1+%2B+B2%29&qdata_type=NUMBERS+OF+FISH&qoutput_type=TABLE&qsource=PRODUCTION"
# )
# save(mock_html_with_data, file = here::here("tests/testthat/mock_html_with_data.rds"))
#
# mock_html_no_data <- httr::GET(
#   "https://www.st.nmfs.noaa.gov/SASStoredProcess/guest?_program=%2F%2FFoundation%2FSTP%2Fmrip_series_catch&qyearfrom=1981&qyearto=2024&qsummary=cumulative_pya&qwave=1&fshyr=annual&qstate=NORTH+AND+MID-ATLANTIC&qspecies=PACIFIC%20COD&qmode_fx=ALL+MODES+COMBINED&qarea_x=ALL+AREAS+COMBINED&qcatch_type=TOTAL+CATCH+%28TYPE+A+%2B+B1+%2B+B2%29&qdata_type=NUMBERS+OF+FISH&qoutput_type=TABLE&qsource=PRODUCTION"
# )
# save(mock_html_no_data, file = here::here("tests/testthat/mock_html_no_data.rds"))

load(here::here("tests/testthat/mock_html_with_data.rds"))
load(here::here("tests/testthat/mock_html_no_data.rds"))

test_that("get_mrip_catch returns data for a valid query", {
  # Mock httr::GET to return our mock HTML with data
  stub(get_mrip_catch, "httr::GET(url)", mock_html_with_data)

  result <- get_mrip_catch(species = "black sea bass")

  # Check if the output is a list with the expected names
  expect_type(result, "list")
  expect_named(result, c("data", "metadata", "url"))

  # Check the data component
  expect_s3_class(result$data, "data.frame")
  expect_equal(nrow(result$data), 44)
  expect_named(
    result$data,
    c(
      'Estimate Status',
      'Year',
      'Fishing Year',
      'Common Name',
      'Cumulative Through',
      'PSE Total Catch (A+B1+B2)',
      'Does Total Catch (A+B1+B2) Meet MRIP Standard',
      'Is Total Catch (A+B1+B2) Significantly Different From 0',
      'Total Catch (A+B1+B2)',
      'Total Catch (A+B1+B2) Lower 95% Confidence Limit',
      'Total Catch (A+B1+B2) Upper 95% Confidence Limit',
      '** Contribution of Imputed Data to Total Catch Rate',
      'SPECIES',
      'REGION'
    )
  )
  expect_equal(unique(result$data$SPECIES), "BLACK SEA BASS")
  expect_equal(unique(result$data$REGION), "NORTH AND MID-ATLANTIC")
})


test_that("get_mrip_catch handles a 'no records' query gracefully", {
  # Mock httr::GET to return our mock HTML with no data
  stub(get_mrip_catch, "httr::GET(url)", mock_html_no_data)

  result <- get_mrip_catch(species = "non-existent fish")

  # Check if the output is a list with the expected names
  expect_type(result, "list")
  expect_named(result, c("data", "metadata", "url"))

  # Check the data component for the specific message
  expect_equal(result$data, "No records matched your query parameters")

  # Check the metadata component
  expect_s3_class(result$metadata, "data.frame")
  expect_true(stringr::str_detect(
    paste(result$metadata[, 1]),
    "No records matched your query parameters"
  ))
})

test_that("get_mrip_catch stops on unexpected page format", {
  # Mock the rvest::html_table function directly to return a list with fewer than 3 elements.
  # This bypasses the need to mock the entire network request.
  stub(get_mrip_catch, "rvest::html_table", function(...) {
    list(data.frame(x = 1), data.frame(x = 2))
  })

  # The function should throw an error with the specific message
  # Note: I've updated the regex slightly to be more flexible, just in case.
  expect_error(
    get_mrip_catch(species = "any"),
    "Unexpected format on the MRIP query tool page"
  )
})
