library(dplyr)
library(testthat)
library(data.table)

df <- data.table(forecast = c(rep(.1, 5), rep(.2, 5), rep(.4, 5),
                              rep(.5, 2), rep(.9, 2), 1.0))

test_that("trimmed mean works", {
  # Untrimmed, the mean is .345. Trimmed, it's .325.
  expect_equal(trim(df), .325)
}
)

test_that("trimmed mean works with p arg", {
  # Same vector but trimming top and bottom 25% (leaving middle 50%).
  expect_equal(trim(df, .25), .3)
}
)

test_that("P(a) + P(not a) = 1 always", {
  notdf <- data.table(df)  # copy of df
  notdf$forecast <- 1 - notdf$forecast
  expect_equal(trim(df) + trim(notdf), 1)
  # The other three want vector (not data table) as input.
  vec_not <- notdf$forecast
  vec <- df$forecast
  expect_equal(neymanAggCalc(vec) + neymanAggCalc(vec_not), 1)
  expect_equal(hd_trim(vec) + hd_trim(vec_not), 1)
  expect_equal(geoMeanCalc(vec) + geoMeanCalc(vec_not), 1)
}
)
