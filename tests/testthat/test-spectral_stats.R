# test-spectral_stats.R

library(testthat)
library(tuneR)
library(Rthoptera)

# Create a simple test Wave object
test_wave <- sine(440)  # Sine wave at 440Hz as test signal

test_that("spectral_stats runs without errors", {
  result <- spectral_stats(test_wave, specimen.id = "Test_Specimen")

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("spectral_stats output contains correct data columns", {
  result <- spectral_stats(test_wave, specimen.id = "Test_Specimen")

  expect_true(nrow(result$data) == 1)
  expect_equal(ncol(result$data), 15)  # Adjust based on the number of columns in the output data frame
})

test_that("spectral_stats plot is a plotly object", {
  result <- spectral_stats(test_wave, specimen.id = "Test_Specimen")

  expect_s3_class(result$plot, "plotly")
})
