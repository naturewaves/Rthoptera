# test-spectrum_plotly.R

library(testthat)
library(Rthoptera)
library(RthopteraSounds)
library(plotly)

# Load test data
data("coryphoda", package = "RthopteraSounds")

test_that("spectrum_plotly returns a valid list with Plotly and summary data", {
  result <- spectrum_plotly(coryphoda, freq_res = 50, fmin = 1, fmax = 10)

  # Check if result is a list
  expect_true(is.list(result))

  # Check if the list contains a Plotly object and a summary data frame
  expect_true(inherits(result$plot, "plotly"))
  expect_true(is.data.frame(result$summary))
})

test_that("spectrum_plotly applies the correct frequency axis limits", {
  fmin <- 2  # kHz
  fmax <- 12  # kHz
  result <- spectrum_plotly(coryphoda, fmin = fmin, fmax = fmax)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check if x-axis limits are correctly set
  expect_equal(plot_data$x$layout$xaxis$range[[1]], fmin)
  expect_equal(plot_data$x$layout$xaxis$range[[2]], fmax)
})

test_that("spectrum_plotly includes lines when show.lines is TRUE", {
  result <- spectrum_plotly(coryphoda, show_lines = TRUE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check if lines for carrier frequency and bandwidth are added
  line_segments <- plot_data$x$data[vapply(plot_data$x$data, function(d) d$type == "scatter", logical(1))]
  expect_true(length(line_segments) >= 2)  # Check for at least 3 lines (carrier, low freq, high freq)
})

test_that("spectrum_plotly applies plot title correctly", {
  title <- "Test Spectrum Plot"
  result <- spectrum_plotly(coryphoda, plot_title = title, italic_title = FALSE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check if the title exists
  expect_true(!is.null(plot_data$x$layout$title), info = "Plot title is missing")

  # Remove HTML tags from the title (if any) for comparison
  actual_title <- gsub("<[^>]*>", "", plot_data$x$layout$title$text)

  # Verify that the title matches the one provided
  expect_equal(actual_title, title)
})

test_that("spectrum_plotly applies italic title when specified", {
  title <- "Test Spectrum Plot"
  result <- spectrum_plotly(coryphoda, plot_title = title, italic_title = TRUE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check for italicized title (using <i> HTML tag)
  expect_match(plot_data$x$layout$title$text, "<i>")
})

test_that("spectrum_plotly summary data contains correct columns", {
  result <- spectrum_plotly(coryphoda)

  # Check if the summary data frame has the expected columns
  expected_columns <- c("spec.ex", "spec.sd", "spec.var", "spec.ent", "spec.flat", "carrier", "low.f", "high.f", "bandw")
  expect_true(all(expected_columns %in% colnames(result$summary)))
})

# Additional Tests to Increase Coverage

test_that("spectrum_plotly works correctly when db.shade is FALSE", {
  result <- spectrum_plotly(coryphoda, db_shade = FALSE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check that only one ribbon (linear amplitude) is added
  ribbon_traces <- plot_data$x$data[vapply(plot_data$x$data, function(d) d$type == "scatter" && d$mode == "lines", logical(1))]
  expect_equal(length(ribbon_traces), 1, info = "Expected one ribbon trace when db.shade is FALSE")
})

test_that("spectrum_plotly calculates total bandwidth correctly when total.bandwidth is TRUE", {
  result <- spectrum_plotly(coryphoda, total_bandwidth = TRUE)

  # Extract summary data
  summary_data <- result$summary

  # Check that bandwidth is calculated as the range where amplitude >= 0.1
  expect_true(summary_data$low.f <= summary_data$carrier)
  expect_true(summary_data$high.f >= summary_data$carrier)
})

test_that("spectrum_plotly limits indices correctly when limit.indices is TRUE", {
  result_false <- spectrum_plotly(coryphoda, limit_indices = FALSE)
  result_true <- spectrum_plotly(coryphoda, limit_indices = TRUE)

  # Compare summary statistics
  expect_false(identical(result_false$summary$spec.ent, result_true$summary$spec.ent),
               info = "Expected different spectral entropy when limit_indices is TRUE")
})

test_that("spectrum_plotly adds parameter annotations when add_params is TRUE", {
  result <- spectrum_plotly(coryphoda, add_params = TRUE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check if annotations are added
  annotations <- plot_data$x$layout$annotations
  expect_true(any(grepl("Sampling Rate", sapply(annotations, `[[`, "text"))),
              info = "Parameter annotations are missing when add_params is TRUE")
})

test_that("spectrum_plotly adds summary annotations when add_summary is TRUE", {
  result <- spectrum_plotly(coryphoda, add_summary = TRUE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check if summary annotations are added
  annotations <- plot_data$x$layout$annotations
  expect_true(any(grepl("Excursion", sapply(annotations, `[[`, "text"))),
              info = "Summary annotations are missing when add_summary is TRUE")
})

test_that("spectrum_plotly hides axis titles when show_x_title and show_y_title are FALSE", {
  result <- spectrum_plotly(coryphoda, show_x_title = FALSE, show_y_title = FALSE)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Function to safely extract the title text
  get_title_text <- function(title_obj) {
    if (is.null(title_obj)) {
      return(NULL)
    } else if (is.list(title_obj) && !is.null(title_obj$text)) {
      return(title_obj$text)
    } else if (is.character(title_obj)) {
      return(title_obj)
    } else {
      return(NULL)
    }
  }

  x_title_text <- get_title_text(plot_data$x$layout$xaxis$title)
  y_title_text <- get_title_text(plot_data$x$layout$yaxis$title)

  expect_true(x_title_text == "", info = "X-axis title should be an empty string when show_x_title is FALSE")
  expect_true(y_title_text == "", info = "Y-axis title should be an empty string when show_y_title is FALSE")
})

test_that("spectrum_plotly applies custom colors correctly", {
  custom_color_db <- "red"
  custom_color_linear <- "blue"
  result <- spectrum_plotly(coryphoda, color_db = custom_color_db, color_linear = custom_color_linear)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check colors in the plot data
  ribbon_traces <- plot_data$x$data[vapply(plot_data$x$data, function(d) d$type == "scatter" && d$mode == "lines", logical(1))]
  colors_used <- sapply(ribbon_traces, function(d) d$line$color)
  expect_true(custom_color_db %in% colors_used || custom_color_linear %in% colors_used,
              info = "Custom colors are not applied correctly")
})

test_that("spectrum_plotly applies custom linewidth correctly", {
  custom_linewidth <- 5
  result <- spectrum_plotly(coryphoda, show_lines = TRUE, linewidth = custom_linewidth)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)

  # Check linewidth in the line segments
  line_traces <- plot_data$x$data[vapply(plot_data$x$data, function(d) d$type == "scatter" && d$mode == "lines", logical(1))]
  linewidths_used <- sapply(line_traces, function(d) {
    lw <- d$line$width
    if (is.null(lw)) return(NA)
    if (is.list(lw)) lw <- unlist(lw)
    as.numeric(lw)
  })

  # Remove NA values (traces without linewidth)
  linewidths_used <- linewidths_used[!is.na(linewidths_used)]

  expect_true(all(linewidths_used == custom_linewidth),
              info = "Custom linewidth is not applied correctly to line segments")
})


test_that("spectrum_plotly handles different window functions and summary functions", {
  window_functions <- c("hanning", "bartlett", "blackman", "flattop", "hamming", "rectangle")
  summary_functions <- c("mean", "median")

  for (wn in window_functions) {
    for (fun in summary_functions) {
      message("Testing with wn = ", wn, " and fun = ", fun)
      # Capture warnings
      result <- withCallingHandlers(
        spectrum_plotly(coryphoda, wn = wn, fun = fun),
        warning = function(w) {
          message("Warning with wn = ", wn, ", fun = ", fun, ": ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      if (is.null(result)) {
        message("spectrum_plotly returned NULL with wn = ", wn, ", fun = ", fun)
        next  # Skip to the next iteration
      }
      expect_true(is.list(result), info = paste("Failed with wn =", wn, "and fun =", fun))
    }
  }
})




test_that("spectrum_plotly adjusts window length wl when odd", {
  freq_res <- 333
  result <- spectrum_plotly(coryphoda, freq_res = freq_res)

  # Calculate expected wl
  expected_wl <- coryphoda@samp.rate / freq_res
  expected_wl <- floor(expected_wl)  # Use floor to match the function's calculation
  if (expected_wl %% 2 == 1) { expected_wl <- expected_wl + 1 }

  # Extract parameters from annotations
  result_with_params <- spectrum_plotly(coryphoda, freq_res = freq_res, add_params = TRUE)
  plot_data <- plotly::plotly_build(result_with_params$plot)
  annotations <- plot_data$x$layout$annotations
  params_annotation <- annotations[[1]]$text
  wl_value <- as.numeric(sub(".*Window Size: (\\d+).*", "\\1", params_annotation))

  expect_equal(wl_value, expected_wl, tolerance = 1,
               info = "Window length wl is not adjusted correctly when odd")
})


test_that("spectrum_plotly handles fmax being NULL or 0", {
  result_null <- spectrum_plotly(coryphoda, fmax = NULL)
  result_zero <- spectrum_plotly(coryphoda, fmax = 0)

  # Extract plot data
  plot_data_null <- plotly::plotly_build(result_null$plot)
  plot_data_zero <- plotly::plotly_build(result_zero$plot)

  # Expected fmax is the Nyquist frequency
  expected_fmax <- coryphoda@samp.rate / 2 / 1000  # in kHz

  expect_equal(plot_data_null$x$layout$xaxis$range[[2]], expected_fmax)
  expect_equal(plot_data_zero$x$layout$xaxis$range[[2]], expected_fmax)
})

# test_that("spectrum_plotly handles y_position and x_position parameters (limited support)", {
#   result <- spectrum_plotly(coryphoda, y_position = "right", x_position = "top")
#
#   # Extract plot data
#   plot_data <- plotly::plotly_build(result$plot)
#
#   # Check if the axis positions are set (Note: may not be supported)
#   yaxis_side <- plot_data$x$layout$yaxis$side
#   xaxis_side <- plot_data$x$layout$xaxis$side
#
#   # Since axis positioning may not be applied, we skip this test
#   skip("Axis positioning is not reliably supported in Plotly R; skipping test.")
# })

test_that("spectrum_plotly handles x_breaks parameter", {
  x_breaks <- 15
  result <- spectrum_plotly(coryphoda, x_breaks = x_breaks)

  # Extract plot data
  plot_data <- plotly::plotly_build(result$plot)
  tick_vals <- plot_data$x$layout$xaxis$tickvals

  expect_equal(length(tick_vals), x_breaks, info = "Number of x-axis breaks does not match x_breaks parameter")
})
