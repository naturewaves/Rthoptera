# test-spectrum_plot.R

library(testthat)
library(tuneR)
library(ggplot2)
library(Rthoptera)
library(RthopteraSounds)

# Load test data
data("coryphoda", package = "RthopteraSounds")

test_that("spectrum_plot generates a valid ggplot object", {
  result <- spectrum_plot(coryphoda, win.size = 512, fmin = 1, fmax = 10)

  # Check if result is a list with the ggplot object
  expect_true(is.list(result))
  expect_true(inherits(result[[1]], "ggplot"))

  # Test the plot's x-axis limits
  plot_build <- ggplot_build(result[[1]])
  plot_data <- plot_build$data[[1]]

  # Verify the x-axis data exists and matches expected range
  expect_true(!is.null(plot_data$x))
  expect_true(min(plot_data$x, na.rm = TRUE) >= 1)   # fmin = 1 kHz
  expect_true(max(plot_data$x, na.rm = TRUE) <= 10)  # fmax = 10 kHz
})


test_that("spectrum_plot flips the plot when specified", {
  result <- spectrum_plot(coryphoda, flip = TRUE)

  # Check if the plot has been flipped
  expect_true("CoordFlip" %in% class(result[[1]]$coordinates))
})
