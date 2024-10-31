# test-multiplot_ggplot.R

library(testthat)
library(tuneR)
library(RthopteraSounds)
library(Rthoptera)

# Load the coryphoda data from RthopteraSounds package
data(coryphoda)

test_that("multiplot_ggplot runs without errors", {
  result <- multiplot_ggplot(coryphoda)

  expect_s3_class(result, "patchwork")
})

test_that("multiplot_ggplot handles cutoff and overlap", {
  result <- multiplot_ggplot(coryphoda, cutoff = -50, overlap = 90)

  expect_s3_class(result, "patchwork")
})

test_that("multiplot_ggplot handles custom oscillo_label", {
  result <- multiplot_ggplot(coryphoda, oscillo_label = "Custom Label")

  expect_s3_class(result, "patchwork")
})
