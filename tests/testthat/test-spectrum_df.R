# test-spectrum_df.R

library(testthat)
library(Rthoptera)
library(tuneR)
library(tibble)

# Load test data
data("coryphoda", package = "RthopteraSounds")

test_that("spectrum_df returns valid data frames", {
  result <- spectrum_df(coryphoda)

  # Check if the result is a list
  expect_true(is.list(result))

  # Check if both spec_df and params_df are tibbles
  expect_true(inherits(result$spec_df, "tbl_df"))
  expect_true(inherits(result$params_df, "tbl_df"))

  # Check if spec_df has two columns: frequency and amplitude
  expect_equal(colnames(result$spec_df), c("frequency", "amplitude"))

  # Check if params_df contains the expected parameters
  expect_true(all(c("srate", "freq.res", "wl", "wn", "fun") %in% colnames(result$params_df)))
})

test_that("spectrum_df computes valid frequency and amplitude values", {
  result <- spectrum_df(coryphoda)

  # Check if frequency values are positive
  expect_true(all(result$spec_df$frequency >= 0))

  # Check if amplitude values are numeric
  expect_true(is.numeric(result$spec_df$amplitude))
})

test_that("spectrum_df handles different frequency resolutions", {
  result_10hz <- spectrum_df(coryphoda, freq_res = 10)
  result_20hz <- spectrum_df(coryphoda, freq_res = 20)

  # Check that window length changes with frequency resolution
  expect_true(result_10hz$params_df$wl > result_20hz$params_df$wl)
})
