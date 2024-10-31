# test-meanspectrum_plotly.R

library(testthat)
library(RthopteraSounds)
library(Rthoptera)

test_that("meanspectrum_plotly generates a valid plotly object", {
  # Load the coryphoda data
  data(coryphoda)

  # Run the meanspectrum_plotly function
  result <- meanspectrum_plotly(coryphoda)

  # Check if the result is a valid plotly object
  expect_true(inherits(result, "plotly"))

  # Check that the plot contains data for the mean amplitude
  plot_data <- plotly::plotly_data(result)
  expect_true("mean_amp" %in% colnames(plot_data))
  expect_true("freq" %in% colnames(plot_data))

  # Ensure frequencies and amplitudes are present and valid
  expect_true(all(plot_data$freq >= 0))
  expect_true(all(plot_data$mean_amp >= 0))
})
