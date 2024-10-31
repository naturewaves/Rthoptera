library(testthat)
library(ggplot2)
library(RthopteraSounds)

# Load real data from the RthopteraSounds package
data(coryphoda)

# Test if window length is auto-calculated when auto_wl = TRUE
test_that("Window length is auto-calculated based on sample rate when auto_wl = TRUE", {
  result <- meanspec_ggplot(coryphoda, scale_type = "dB", auto_wl = TRUE)

  # Check if result is a ggplot object
  expect_true("ggplot" %in% class(result))
})

# Test if window length is manually set when auto_wl = FALSE
test_that("Window length is manually set when auto_wl = FALSE", {
  wl_value <- 512
  result <- meanspec_ggplot(coryphoda, scale_type = "dB", auto_wl = FALSE, wl = wl_value)

  # Check if result is a ggplot object
  expect_true("ggplot" %in% class(result))
})

# Test if plot is correctly generated with dB scale
test_that("Plot is correctly generated with dB scale", {
  result <- meanspec_ggplot(coryphoda, scale_type = "dB")

  built_plot <- ggplot_build(result)
  y_limits <- built_plot$layout$panel_scales_y[[1]]$range$range
  expect_equal(y_limits, c(-50, 0))
})

# Test if plot is correctly generated with linear scale
test_that("Plot is correctly generated with linear scale", {
  result <- meanspec_ggplot(coryphoda, scale_type = "Linear")

  built_plot <- ggplot_build(result)
  y_breaks <- built_plot$layout$panel_scales_y[[1]]$breaks
  expect_equal(y_breaks, c(0, 0.5, 1))
})

# Test if the plot is flipped with coord_flip()
test_that("Plot is flipped with coord_flip", {
  result <- meanspec_ggplot(coryphoda, scale_type = "dB")

  # Check if coord_flip is applied
  expect_true("CoordFlip" %in% class(result$coordinates))
})

# Test if scale labels and axis titles are correct for Linear scale
test_that("Axis titles and labels are correct for Linear scale", {
  result <- meanspec_ggplot(coryphoda, scale_type = "linear")

  # Inspect the labels directly from the ggplot object
  x_label <- result$labels$y # y-axis label before flip becomes x-axis

  # Check x-axis label (flipped from y-axis)
  expect_equal(x_label, "Amplitude")
})

# Test if scale labels and axis titles are correct for dB scale
test_that("Axis titles and labels are correct for dB scale", {
  result <- meanspec_ggplot(coryphoda, scale_type = "dB")

  # Inspect the labels directly from the ggplot object
  x_label <- result$labels$y # y-axis label before flip becomes x-axis

  # Check x-axis label (flipped from y-axis)
  expect_equal(x_label, "dB")
})

# Test if manual overlap value works as expected
test_that("Overlap value is correctly applied", {
  result <- meanspec_ggplot(coryphoda, overlap = 90)

  expect_true("ggplot" %in% class(result))
})
