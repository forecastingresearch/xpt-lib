library(dplyr)
library(testthat)
library(data.table)

forecasts <- c(rep(.1, 5), rep(.2, 5), rep(.4, 5), rep(.5, 2), rep(.9, 2), 1.0)

test_that("trimmed mean works", {
  # Untrimmed, the mean is .345. Trimmed, it's .325.
  expect_equal(trim(forecasts), .325)
}
)

test_that("trimmed mean works with p arg", {
  # Same vector but trimming top and bottom 25% (leaving middle 50%).
  expect_equal(trim(forecasts, .25), .3)
}
)

test_that("P(a) + P(not a) = 1 always", {
  not_forecasts <- 1 - forecasts
  expect_equal(trim(forecasts) + trim(not_forecasts), 1)
  expect_equal(neymanAggCalc(forecasts*100) + neymanAggCalc(not_forecasts*100), 100)
  expect_equal(hd_trim(forecasts) + hd_trim(not_forecasts), 1)
  # GeoMean doesn't have this property
}
)
