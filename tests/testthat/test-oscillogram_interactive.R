# test-oscillogram_interactive.R

library(testthat)
library(plotly)
library(RthopteraSounds)
library(Rthoptera)

# Load test data
data("coryphoda")

test_that("oscillogram_interactive generates a valid Plotly object", {
  p <- oscillogram_interactive(coryphoda, title = "Test Oscillogram", line_color = "blue")

  # Check if the returned object is a plotly object
  expect_true(inherits(p, "plotly"))


})

