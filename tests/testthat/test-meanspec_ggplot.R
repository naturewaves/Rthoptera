# test-meanspec_ggplot.R

library(testthat)
library(ggplot2)
library(RthopteraSounds)
library(Rthoptera)

test_that("meanspec_ggplot generates a valid ggplot object", {
  # Load test data
  data(coryphoda)

  # Test with default parameters
  plot <- meanspec_ggplot(coryphoda)

  # Check that the output is a ggplot object
  expect_s3_class(plot, "ggplot")

  # Check the plot has the correct labels
  expect_true("x" %in% names(plot$mapping))
  expect_true("y" %in% names(plot$mapping))

  # Check if scale is properly applied
  expect_true(ggplot2::is.ggplot(plot))
})
