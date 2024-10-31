# test-broadband_activity.R
library(testthat)
library(Rthoptera)
library(RthopteraSounds)
library(tuneR)

# Load the coryphoda data from RthopteraSounds
data("coryphoda", package = "RthopteraSounds")

test_that("broadband_activity function processes mono wave and returns expected output structure", {
  # Use coryphoda sample, which is mono
  wave_mono <- coryphoda

  # Run broadband_activity on mono wave
  result <- broadband_activity(wave_mono, channel = "left",
                               hpf = 500, rm.offset = TRUE,
                               freq.res = 50, cutoff = -60,
                               spectrogram = FALSE)

  # Test output structure
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("index", "channel", "value", "nclicks") %in% colnames(result)))
  expect_equal(result$index[1], "bbai")
})

test_that("broadband_activity correctly processes stereo wave and handles channel selection", {
  # Convert coryphoda to stereo
  wave_stereo <- stereo(coryphoda, coryphoda)

  # Test each channel
  result_left <- broadband_activity(wave_stereo, channel = "left", hpf = 500, rm.offset = TRUE, freq.res = 50, cutoff = -60, spectrogram = FALSE)
  result_right <- broadband_activity(wave_stereo, channel = "right", hpf = 500, rm.offset = TRUE, freq.res = 50, cutoff = -60, spectrogram = FALSE)
  result_mix <- broadband_activity(wave_stereo, channel = "mix", hpf = 500, rm.offset = TRUE, freq.res = 50, cutoff = -60, spectrogram = FALSE)

  # Check output structures
  expect_s3_class(result_left, "tbl_df")
  expect_s3_class(result_right, "tbl_df")
  expect_s3_class(result_mix, "tbl_df")

  # Check channel selection in summary
  expect_equal(result_left$channel[1], "left")
  expect_equal(result_right$channel[1], "right")
  expect_equal(result_mix$channel[1], "mix")
})

test_that("broadband_activity function handles DC offset removal without errors", {
  # Create a wave with simulated DC offset
  wave_offset <- coryphoda
  wave_offset@left <- wave_offset@left + 100  # Simulate DC offset

  # Run broadband_activity with and without DC offset removal
  result_with_offset <- broadband_activity(wave_offset, rm.offset = TRUE, spectrogram = FALSE)
  result_no_offset <- broadband_activity(wave_offset, rm.offset = FALSE, spectrogram = FALSE)

  # Check that both results return the expected tibble structure
  expect_s3_class(result_with_offset, "tbl_df")
  expect_s3_class(result_no_offset, "tbl_df")

  # Ensure both outputs are identical, as DC offset removal does not impact the calculated values in this function
  expect_equal(result_with_offset, result_no_offset,
               info = "Expected identical results with and without DC offset removal")
})

test_that("broadband_activity correctly applies cutoff and click detection criteria", {
  # Test default and adjusted click height
  result_default <- broadband_activity(coryphoda, click.length = 10,
                                       difference = 10, gap.allowance = 2,
                                       spectrogram = FALSE)
  result_high_click_height <- broadband_activity(coryphoda, click.length = 20,
                                                 spectrogram = FALSE)

  # Expect fewer clicks detected with higher click height
  expect_gt(result_default$nclicks, result_high_click_height$nclicks)
})

test_that("broadband_activity handles spectrogram plotting with click overlay correctly", {
  # Run broadband_activity with spectrogram = TRUE and dark.plot = TRUE
  result_plot <- broadband_activity(coryphoda, spectrogram = TRUE,
                                    dark.plot = TRUE, plot.title = "Test Plot")

  # Test that output includes a ggplot object
  expect_true("ggplot" %in% class(result_plot$spectrogram))
})

test_that("broadband_activity handles invalid inputs gracefully", {
  # Test with an invalid channel option
  expect_error(
    broadband_activity(coryphoda, channel = "invalid_channel"),
    regexp = "Invalid channel selected"
  )
  # Test with a negative high-pass filter value
  expect_error(broadband_activity(coryphoda, hpf = -1), "HPF should be either 0 or a positive number")
})


test_that("broadband_activity processes each channel in stereo data correctly", {
  wave_stereo <- stereo(coryphoda, coryphoda)
  result_each <- broadband_activity(wave_stereo, channel = "each", hpf = 500,
                                    rm.offset = TRUE, freq.res = 50,
                                    cutoff = -60, spectrogram = FALSE)

  # Expect a list of results for left and right channels
  expect_type(result_each, "list")
  expect_true(all(c("value_l", "value_r") %in% names(result_each)))

  # Check summary structure
  expect_true(all(c("index", "value_l", "value_r", "value_avg") %in% colnames(result_each)))
  expect_equal(nrow(result_each), 1)
})

test_that("broadband_activity function handles edge cases with extreme parameters", {
  # Edge case with zero click.length
  result_zero_click <- broadband_activity(coryphoda, click.length = 0, difference = 5, spectrogram = FALSE)
  expect_s3_class(result_zero_click, "tbl_df")

  # Very high cutoff threshold, which might yield few or no clicks
  result_high_cutoff <- broadband_activity(coryphoda, cutoff = 0, spectrogram = FALSE)
  expect_s3_class(result_high_cutoff, "tbl_df")

  # Edge case where `gap.allowance` is large, allowing more relaxed click detection
  result_large_gap <- broadband_activity(coryphoda, gap.allowance = 20, spectrogram = FALSE)
  expect_s3_class(result_large_gap, "tbl_df")
})
