# test-song_stats_lq.R

library(testthat)
library(tuneR)
library(RthopteraSounds)
library(Rthoptera)

data("coryphoda")
test_wave <- coryphoda
test_that("song_stats_lq runs without errors", {
  result <- song_stats_lq(test_wave, specimen_id = "Test_Specimen")

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("peak_data" %in% names(result))
  expect_true("train_data" %in% names(result))
  expect_true("motif_data" %in% names(result))
  expect_true("params" %in% names(result))
})

