# test-spectral_stats.R

library(testthat)
library(tuneR)
library(Rthoptera)

# Create a simple test Wave object
test_wave <- sine(440)  # Sine wave at 440Hz as test signal

test_that("spectral_stats runs without errors", {
  result <- spectral_stats(test_wave, specimen_id = "Test_Specimen")

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("spectral_stats output contains correct data columns", {
  result <- spectral_stats(test_wave, specimen_id = "Test_Specimen", temp = 25)

  expect_true(nrow(result$data) == 1)
  expect_equal(ncol(result$data), 19)
  expected_columns <- c(
    "specimen.id", "sound.type", "low.f", "high.f", "bandw", "peak.f",
    "sp.exc", "sp.ene", "sp.ent", "sp.flat", "q.factor",
    "temp", "par.hpf", "par.cutoff", "par.s.rate", "par.wlen",
    "par.freq.res", "par.robust", "par.scale"
  )
  expect_equal(names(result$data), expected_columns)
})

test_that("spectral_stats plot is a plotly object", {
  result <- spectral_stats(test_wave, specimen_id = "Test_Specimen")

  expect_s3_class(result$plot, "plotly")
})

test_that("spectral_stats calculates expected statistics for sine wave", {
  result <- spectral_stats(test_wave, specimen_id = "Test_Specimen")

  # Check that peak frequency is close to the sine wave frequency
  expect_equal(result$data$peak.f, 0.44, tolerance = 0.1)  # Increased tolerance for frequency rounding

  # Check other statistics are within reasonable ranges
  expect_true(result$data$sp.ene > 0)
  expect_true(result$data$sp.exc > 0)
  expect_true(result$data$sp.ent >= 0 && result$data$sp.ent <= 1)
  expect_true(result$data$sp.flat >= 0 && result$data$sp.flat <= 1)
})

test_that("spectral_stats handles additional parameters correctly", {
  result <- spectral_stats(
    test_wave, specimen_id = "Test_Specimen", db = FALSE,
    cutoff = 0.1, robust = TRUE, hpf = 1
  )

  # Confirm output includes expected parameter values
  expect_equal(result$data$par.scale, "linear")
  expect_equal(result$data$par.cutoff, 0.1)
  expect_equal(result$data$par.hpf, 1)
  expect_true(result$data$par.robust)
})
