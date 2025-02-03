library(testthat)
library(Rthoptera)
library(RthopteraSounds)
library(seewave)

# Load example data
data(coryphoda)

# Test suite for clean_wave function
test_that("clean_wave function works correctly", {

  # Test 1: Basic functionality with default parameters
  test_that("Default parameters work", {
    result <- clean_wave(coryphoda)
    expect_s4_class(result, "Wave")
    expect_true(is.logical(result@stereo))
    expect_true(is.numeric(result@left))
  })

  # Test 2: FIR filter
  test_that("FIR filter works", {
    result <- clean_wave(coryphoda, filter = "fir", min = 100, max = 1000)
    expect_s4_class(result, "Wave")
  })

  # Test 3: Butterworth filter
  test_that("Butterworth filter works", {
    result <- clean_wave(coryphoda, filter = "bwf", min = 0.1, max = 0.8)
    expect_s4_class(result, "Wave")
  })

  # Test 4: Frequency filter
  test_that("Frequency filter works", {
    result <- clean_wave(coryphoda, filter = "freq", min = 100, max = 1000)
    expect_s4_class(result, "Wave")
  })

  # Test 5: Invalid filter type
  test_that("Invalid filter type throws an error", {
    expect_error(clean_wave(coryphoda, filter = "invalid_filter"), "Invalid filter input")
  })

  # Test 6: Noise gate functionality
  test_that("Noise gate works", {
    result <- clean_wave(coryphoda, gate = 0.1)
    expect_s4_class(result, "Wave")
  })

  # Test 7: Oscillogram plotting
  test_that("Oscillogram plotting works", {
    expect_silent(clean_wave(coryphoda, oscillo = TRUE))
  })

  # Test 8: No oscillogram plotting
  test_that("No oscillogram plotting works", {
    expect_silent(clean_wave(coryphoda, oscillo = FALSE))
  })

  # Test 9: min and max frequencies
  test_that("min and max frequencies are correctly applied", {
    result <- clean_wave(coryphoda, min = 100, max = 1000)
    expect_s4_class(result, "Wave")
    expect_true(result@samp.rate / 2 >= 1000)  # Ensure max frequency is correctly applied
  })

  # Test 10: min frequency default
  test_that("min frequency defaults to 0", {
    result <- clean_wave(coryphoda, max = 1000)
    expect_s4_class(result, "Wave")
    expect_true(result@samp.rate / 2 >= 1000)
  })

  # Test 11: max frequency default (Nyquist frequency)
  test_that("max frequency defaults to Nyquist frequency", {
    result <- clean_wave(coryphoda, min = 100)
    expect_s4_class(result, "Wave")
    expect_true(result@samp.rate / 2 == max(result@samp.rate / 2))
  })

})
