# Load the necessary libraries
library(testthat)
library(Rthoptera) # Replace with your actual package name
library(shiny)
library(mockery)

# Test if available apps are listed when no app_name is provided
test_that("launch_app() lists available apps when app_name is NULL", {
  apps <- launch_app() # No app_name provided

  expect_type(apps, "character") # Expect a character vector of app names
  expect_true(length(apps) > 0) # Expect some apps to be available
})

# Test if the function throws an error when no matching app is found
test_that("launch_app() throws error when no matching app is found", {
  expect_error(launch_app("non_existent_app"),
    "No matching app found",
    fixed = TRUE
  )
})

# Simulate multiple apps for testing
test_that("launch_app() throws error when multiple apps match", {
  # Mock available apps to simulate multiple matching cases
  available_apps <- c("app1", "app1_version2")

  # Stub list.dirs to return the mocked available apps
  stub(launch_app, "list.dirs", available_apps)

  expect_error(launch_app("app1"), "Multiple apps match", fixed = TRUE)
})

# Test if the function successfully launches the correct app
test_that("launch_app() launches the correct app when exact match is found", {
  # Mock shiny::runApp to test app launching without actually running it
  mock_runApp <- mockery::mock()

  # Stub list.dirs to return a single matching app
  available_apps <- c("specific_app_name")
  stub(launch_app, "list.dirs", available_apps)

  # Stub shiny::runApp to simulate app launch
  stub(launch_app, "shiny::runApp", mock_runApp)

  # Call launch_app and check if the correct app is "launched"
  launch_app("specific_app_name")

  # Verify that shiny::runApp was called with the correct arguments
  expect_called(mock_runApp, 1)
  expect_args(mock_runApp, 1, file.path(system.file("shiny", package = "Rthoptera"), "specific_app_name"), "normal")
})
