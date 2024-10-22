# test-wave_df.R

library(testthat)
library(Rthoptera)
library(tuneR)
library(dplyr)
library(tibble)

# Load test data
data("coryphoda", package = "RthopteraSounds")

test_that("wave_df returns a valid tibble", {
  df <- wave_df(coryphoda)

  # Check if the result is a tibble
  expect_true(inherits(df, "tbl_df"))

  # Check that the data frame has two columns: "amplitude" and "time"
  expect_equal(colnames(df), c("amplitude", "time"))

  # Check that the amplitude values are within the range [-1, 1]
  expect_true(all(df$amplitude >= -1 & df$amplitude <= 1))
})

test_that("wave_df returns valid time values", {
  df <- wave_df(coryphoda)

  # Check that time starts from 0 and increases
  expect_equal(min(df$time), 0)
  expect_true(all(diff(df$time) > 0))

  # Check that time values correspond to the sample rate
  expected_max_time <- (nrow(df) - 1) / coryphoda@samp.rate
  expect_equal(max(df$time), expected_max_time, tolerance = 1e-6)
})
