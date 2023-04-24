library(dplyr)
library(testthat)
library(data.table)

df <- data.table(forecast = c(rep(.1, 5), rep(.2, 5), rep(.4, 5), rep(.5, 3), rep(1.0, 2)))

test_that("trimmed mean works", {
  # Untrimmed, the mean is .35. Trimmed, it's exactly .3.
  expect_equal(trim(df), .3)
}
)

test_that("trimmed mean works with p arg", {
  # Same vector but trimming top and bottom 25% (leaving middle 50%).
  expect_equal(trim(df, .25), .4)
}
)

test_that("P(a) + P(not a) = 1 always", {
  notdf <- data.frame(df)  # copy of df
  notdf$forecast <- 1 - notdf$forecast
  expect_equal(trim(df) + trim(notdf), 1)
  expect_equal(neymanAggCalc(df) + neymanAggCalc(notdf), 1)
  expect_equal(hd_trim(df) + hd_trim(notdf), 1)
  expect_equal(geoMeanCalc(df) + geoMeanCalc(notdf), 1)
}
)
