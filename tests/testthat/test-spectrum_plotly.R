library(testthat)
library(Rthoptera)
library(RthopteraSounds)

test_that("spectrum_plotly generates a valid Plotly object", {
  result <- spectrum_plotly(coryphoda)

  # Check if the plot object is generated
  expect_true(!is.null(result$plot))


})
