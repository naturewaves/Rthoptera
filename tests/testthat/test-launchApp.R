# test-launchApp.R

library(testthat)
library(Rthoptera)  # Assuming your package is named 'Rthoptera'

test_that("launchApp lists available apps when no app_name is provided", {
  # Test when no app_name is provided
  available_apps <- launchApp()

  # Ensure that some real apps from the package are in the list
  expect_true("bandpass_filter" %in% available_apps)
  expect_true("spectrogram" %in% available_apps)
  expect_true("oscillogram_zoom" %in% available_apps)
})

test_that("launchApp gives an error if app_name does not match any apps", {
  # Expect an error when no apps match
  expect_error(launchApp("nonexistent_app"), "No matching app found for 'nonexistent_app'.")
})

test_that("launchApp handles partial matching of app names", {
  # Test partial matching
  expect_silent(launchApp("spectro"))  # Should match 'spectrogram' without error
  expect_error(launchApp("multi"), "Multiple apps match 'multi'.")  # Should throw error for multiple matches
})

test_that("launchApp launches a specific app", {
  # Mock shiny::runApp to test app launch without actually launching
  with_mocked_bindings(
    {
      shiny::runApp <- function(appDir, display.mode) {
        # We are expecting the directory path for the specific app
        expect_true(grepl("spectrogram", appDir))
        expect_equal(display.mode, "normal")
      }

      # Test launching the 'spectrogram' app
      launchApp("spectrogram")
    }
  )
})
