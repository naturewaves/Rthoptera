# test-merge_waves.R

library(testthat)
library(tuneR)

test_that("merge_waves correctly merges two wave objects", {
  # Create two test wave objects
  wave1 <- sine(440, duration = 1, samp.rate = 44100)
  wave2 <- sine(880, duration = 1, samp.rate = 44100)

  # Merge the wave objects
  merged_wave <- merge_waves(list(wave1, wave2))

  # Check if the merged wave is a Wave object
  expect_s4_class(merged_wave, "Wave")

  # Check if the sample rate is correct
  expect_equal(merged_wave@samp.rate, wave1@samp.rate)

  # Check if the length of the merged wave is correct
  expect_equal(length(merged_wave@left), length(wave1@left) + length(wave2@left))

  # Check if the channels are properly merged
  if (wave1@stereo) {
    expect_equal(length(merged_wave@right), length(wave1@right) + length(wave2@right))
  }
})

test_that("merge_waves returns NULL when input is empty", {
  # Test with an empty list
  merged_wave <- merge_waves(list())

  # Check if the result is NULL
  expect_null(merged_wave)
})
