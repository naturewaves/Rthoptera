# test-oscillogram_plotly.R

library(testthat)
library(plotly)
library(RthopteraSounds)
library(Rthoptera)

# Load test data
data("coryphoda")

test_that("oscillogram_plotly generates a valid Plotly object", {
  p <- oscillogram_plotly(coryphoda, title = "Test Oscillogram", line_color = "blue")

  # Check if the returned object is a plotly object
  expect_true(inherits(p, "plotly"))


})

