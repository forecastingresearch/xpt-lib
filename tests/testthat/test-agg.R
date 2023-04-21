library(dplyr)
library(testthat)
library(data.table)
library(xpt)

df <- data.table(forecast = c(rep(1, 5), rep(2, 5), rep(4, 5), rep(5, 3), rep(10, 2)))

test_that("trimmed mean works", {
  # Untrimmed, the mean is 3.55. Trimmed, it's exactly 3.
  expect_equal(trim(df), 3)
}
)

test_that("trimmed mean works with p arg", {
  # Same vector but trimming top and bottom 25% (leaving middle 50%).
  expect_equal(trim(df, .25), 4)
}
)

test_that("neyman works", {
  # TODO
  expect_equal(neyman(df), 0)
}
)

test_that("HD trim works", {
  # TODO
  expect_equal(hd_trim(df), 0)
})
