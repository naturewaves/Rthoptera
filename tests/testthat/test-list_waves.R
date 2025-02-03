library(testthat)
library(Rthoptera)  # Assuming this is the package where list_waves is located

# Create a temporary directory for testing
test_dir <- tempdir()

# Create some test .wav files in the temporary directory
test_wav_files <- c("test1.wav", "test2.WAV", "test3.txt")
for (file in test_wav_files) {
  file.create(file.path(test_dir, file))
}

# Test suite for list_waves function
test_that("list_waves function works correctly", {

  # Test 1: No folder provided (defaults to working directory)
  test_that("No folder provided defaults to working directory", {
    # Mock the working directory to the test directory
    withr::with_dir(test_dir, {
      result <- list_waves()
      expect_type(result, "character")
      expect_true(all(grepl("\\.wav$", result, ignore.case = TRUE)))
    })
  })

  # Test 2: Valid folder provided
  test_that("Valid folder provided lists .wav files", {
    result <- list_waves(folder = test_dir)
    expect_type(result, "character")
    expect_true(all(grepl("\\.wav$", result, ignore.case = TRUE)))
  })

  # Test 3: No .wav files in the folder
  test_that("No .wav files in the folder returns empty list", {
    # Create an empty directory
    empty_dir <- tempfile()
    dir.create(empty_dir)
    on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

    result <- list_waves(folder = empty_dir)
    expect_type(result, "character")
    expect_length(result, 0)
  })

  # Test 4: Folder does not exist
  test_that("Non-existent folder throws an error", {
    expect_error(list_waves(folder = "nonexistent_folder"), "cannot change working directory")
  })

  # Test 5: Check case-insensitivity of .wav file detection
  test_that("Case-insensitive detection of .wav files", {
    result <- list_waves(folder = test_dir)
    expect_true("test1.wav" %in% result)
    expect_true("test2.WAV" %in% result)
    expect_false("test3.txt" %in% result)
  })

  # Test 6: Check printing of the list
  test_that("List is printed correctly", {
    expect_output(list_waves(folder = test_dir), "1 test1.wav")
    expect_output(list_waves(folder = test_dir), "2 test2.WAV")
  })


})

# Clean up test files
unlink(file.path(test_dir, test_wav_files))
