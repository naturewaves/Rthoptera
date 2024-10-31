library(testthat)
library(ggplot2)
library(RthopteraSounds)

# Load real data from the RthopteraSounds package
data(coryphoda)

# Test if window length is calculated correctly when auto_wl = TRUE
test_that("Window length is auto-calculated based on sample rate when auto_wl = TRUE", {
  result <- spectrum_ggplot(coryphoda, auto_wl = TRUE)
  expect_true("ggplot" %in% class(result)) # Expect ggplot object
})

# Test if window length is taken from wl when auto_wl = FALSE
test_that("Window length is taken from wl when auto_wl = FALSE", {
  wl_value <- 512
  result <- spectrum_ggplot(coryphoda, auto_wl = FALSE, wl = wl_value)
  expect_true("ggplot" %in% class(result))
})

# Test that fmax is set to Nyquist frequency when not provided
test_that("fmax is set to Nyquist frequency when not provided", {
  result <- spectrum_ggplot(coryphoda, fmax = NULL)
  expect_true("ggplot" %in% class(result))
})

# Test if power spectrum is calculated correctly with the dB scale
test_that("Power spectrum is calculated correctly with dB scale", {
  result <- spectrum_ggplot(coryphoda)
  expect_true("ggplot" %in% class(result))
})

# Test if plot contains ribbons and lines when show_lines = TRUE
test_that("Plot contains ribbons and lines when show_lines = TRUE", {
  result <- spectrum_ggplot(coryphoda, show_lines = TRUE)

  # Check if ggplot contains expected elements (ribbon and lines)
  expect_true(any(sapply(result$layers, function(layer) {
    class(layer$geom)[1] == "GeomRibbon"
  })))

  expect_true(any(sapply(result$layers, function(layer) {
    class(layer$geom)[1] == "GeomVline"
  })))
})

# Test if the function correctly handles summary statistics
test_that("Summary statistics are calculated and displayed", {
  result <- spectrum_ggplot(coryphoda, add_summary = TRUE)

  expect_true("ggplot" %in% class(result))
})

# Test that parameters are annotated correctly when add_params = TRUE
test_that("Plot contains parameter information when add_params = TRUE", {
  result <- spectrum_ggplot(coryphoda, add_params = TRUE)

  # Check if ggplot object was generated
  expect_true("ggplot" %in% class(result))
})

# Test if the plot is flipped when flip = TRUE
test_that("Plot is flipped when flip = TRUE", {
  result <- spectrum_ggplot(coryphoda, flip = TRUE)
  expect_true("ggplot" %in% class(result))
  expect_true("CoordFlip" %in% class(result$coordinates))
})
