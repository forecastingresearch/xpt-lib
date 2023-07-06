library(dplyr)
library(testthat)
library(data.table)

forecasts <- c(rep(.1, 5), rep(.2, 5), rep(.4, 5), rep(.5, 2), .8, .9, 1.0)

test_that("trimmed mean works", {
  # Untrimmed, the mean is .36. Trimmed, it's .31875.
  expect_equal(round(trim(forecasts), 2), .32)
}
)

test_that("trimmed mean works with p arg", {
  # Same vector but trimming top and bottom 25% (leaving middle 50%).
  expect_equal(trim(forecasts, .25), .3)
}
)

test_that("P(a) + P(not a) = 1 always", {
  not_forecasts <- 1 - forecasts
  expect_equal(trim(forecasts) + xpt::trim(not_forecasts), 1)
  expect_equal(neymanAggCalc(forecasts*100) + neymanAggCalc(not_forecasts*100), 100)
  expect_equal(hd_trim(forecasts) + hd_trim(not_forecasts), 1)
  # GeoMean doesn't have this property
}
)

test_that("Highest isn't more than 10x lowest", {
  # Make a vector containing each of the aggregation methods on forecasts.
  # The highest value should be no more than 10x the lowest.
  agg_vec <- c(trim(forecasts*100), hd_trim(forecasts*100),
               neymanAggCalc(forecasts*100), geoMeanCalc(forecasts*100),
               geoMeanOfOddsCalc(forecasts*100))
  expect_equal(max(agg_vec) / min(agg_vec) < 10, TRUE)
}
)

test_that("It works when they're all 0", {
  # Trim surely works
  forecasts <- rep(0, 100)
  expect_equal(trim(forecasts), 0)
  # What about geometric mean of odds?
  expect_equal(geoMeanOfOddsCalc(forecasts), 0)
})
