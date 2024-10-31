# test-spectrogram_plotly.R

library(testthat)
library(tuneR)
library(plotly)
library(RthopteraSounds)
library(Rthoptera)

# Load test data
data("coryphoda", package = "RthopteraSounds")

test_that("spectrogram_plotly generates a valid Plotly object", {
  result <- spectrogram_plotly(coryphoda)

  # Check if result is a plotly object
  expect_true(inherits(result, "plotly"))

  # Check that the data contains time and frequency values
  plot_data <- result$x$data[[1]]$z
  expect_true(all(!is.null(plot_data)))
})

test_that("spectrogram_plotly correctly handles the noise floor", {
  result <- spectrogram_plotly(coryphoda, floor = -50)

  # Ensure the noise floor is respected
  plot_data <- result$x$data[[1]]$z
  expect_true(all(plot_data >= -50))
})

test_that("spectrogram_plotly correctly computes spectrogram dimensions", {
  result <- spectrogram_plotly(coryphoda)

  # Ensure that time and frequency values are in expected ranges
  time_range <- range(result$x$data[[1]]$x)
  freq_range <- range(result$x$data[[1]]$y)

  expect_true(time_range[1] >= 0)
  expect_true(freq_range[1] >= 0)
})

test_that("spectrogram_plotly adjusts for different overlap and zero padding values", {
  result_default <- spectrogram_plotly(coryphoda, overlap = 50, zero_padding = 0)
  result_custom <- spectrogram_plotly(coryphoda, overlap = 70, zero_padding = 200)

  # Ensure different results based on overlap and zero padding
  expect_false(identical(result_default$x$data, result_custom$x$data))
})
