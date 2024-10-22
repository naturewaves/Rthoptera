# test-spectrum_ggplot.R

library(testthat)
library(tuneR)
library(seewave)
library(ggplot2)
library(Rthoptera)
library(RthopteraSounds)

# Load sample data
data("coryphoda", package = "RthopteraSounds")

test_that("spectrum_ggplot generates a valid ggplot object", {
  result <- spectrum_ggplot(coryphoda, auto.wl = TRUE)

  # Check that the result is a list containing a ggplot object
  expect_true(is.list(result))
  expect_true(inherits(result[[1]], "ggplot"))

  # Extract plot data and check x-axis range
  plot_build <- ggplot_build(result[[1]])
  plot_data <- plot_build$data[[1]]

  # Check that frequency values are within the expected range
  expect_true(min(plot_data$x) >= 0)
  expect_true(max(plot_data$x) <= coryphoda@samp.rate / 2 / 1000)
})


test_that("spectrum_ggplot flips the plot when specified", {
  result <- spectrum_ggplot(coryphoda, flip = TRUE)

  # Check that the plot is flipped
  expect_true("CoordFlip" %in% class(result[[1]]$coordinates))
})
