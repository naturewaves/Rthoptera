#' Interactive Spectrum Plot using Plotly
#'
#' This function generates an interactive spectrum plot using Plotly, offering
#' customizable options for windowing, overlap, amplitude shading
#' (dB and linear), and the ability to display summary statistics and parameter
#' annotations. The function supports both dB and linear scales, bandwidth
#' calculations, and frequency markers.
#'
#' @param wave A wave object containing the audio data.
#' @param freq_res Numeric, the frequency resolution in Hz per bin. Default is 50.
#' @param db_shade Logical, whether to shade the plot based on decibel amplitude.
#' Default is `TRUE`.
#' @param ovlp Numeric, the overlap between successive windows in percentage.
#' Default is 0 (no overlap).
#' @param fun Character, the summary function to apply to the spectrum, one of
#' "mean", "median", "var", or "sd". Default is "mean".
#' @param wn Character, the window type for the FFT, one of "hanning",
#' "bartlett", "blackman", "flattop", "hamming", or "rectangle". Default is "hanning".
#' @param total_bandwidth Logical, whether to calculate the total bandwidth,
#' ignoring gaps. Default is `FALSE`.
#' @param limit_indices Logical, whether to limit the spectrum indices to the
#' frequency range between `fmin` and `fmax`. Default is `FALSE`.
#' @param plot_title Character, the title of the plot. Default is an empty string.
#' @param italic_title Logical, whether to italicize the plot title. Default is `FALSE`.
#' @param fmin Numeric, minimum frequency (in kHz) to plot. If `NULL`,
#' the entire frequency range is plotted. Default is `NULL`.
#' @param fmax Numeric, maximum frequency (in kHz) to plot. If `NULL`, it
#' defaults to the Nyquist frequency.
#' @param add_params Logical, whether to add window parameters as a label on the plot.
#' Default is `FALSE`.
#' @param add_summary Logical, whether to add summary statistics as a label on
#' the plot. Default is `TRUE`.
#' @param x_breaks Numeric, the number of breaks on the x-axis (frequency axis). Default is 6.
#' @param y_position Character, the position of the y-axis, either "left" or
#' "right". Default is "left".
#' @param x_position Character, the position of the x-axis, either "top" or
#' "bottom". Default is "bottom".
#' @param show_x_title Logical, whether to show the x-axis title. Default is `TRUE`.
#' @param show_y_title Logical, whether to show the y-axis title. Default is `TRUE`.
#' @param color_db Character, color for the dB amplitude plot. Default is "grey30".
#' @param color_linear Character, color for the linear amplitude plot. Default is "black".
#' @param color_carrier Character, color for the carrier frequency marker. Default is "white".
#' @param color_threshold Character, color for the threshold line. Default is "white".
#' @param color_bandwidth Character, color for the bandwidth lines. Default is "white".
#' @param show_lines Logical, whether to show lines for the carrier frequency
#' and bandwidths. Default is `FALSE`.
#' @param linewidth Numeric, the width of the lines in the plot. Default is 1.
#'
#' @return A list including: a Plotly object, and a summary table (data frame).
#' @export
#' @importFrom seewave meanspec spec sh sfm
#' @importFrom plotly plot_ly add_ribbons layout add_segments add_annotations
#' @importFrom scales breaks_pretty
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @examples
#'  \dontrun{
#' # Example usage:
#' spectrum_plotly(Coryphoda_albidicollis, db_shade = FALSE,
#' freq_res = 100, show_lines = TRUE)
#' }
#' `r lifecycle::badge("stable")`
spectrum_plotly <- function(wave,
                            freq_res = 50,
                            db_shade = TRUE,
                            ovlp = 0,
                            fun = "mean",
                            wn = "blackman",
                            total_bandwidth = FALSE,
                            limit_indices = FALSE,
                            plot_title = "",
                            italic_title = TRUE,
                            fmin = 0, fmax = NULL,
                            add_params = FALSE,
                            add_summary = FALSE,
                            x_breaks = 10,
                            y_position = "left",
                            x_position = "bottom",
                            show_x_title = TRUE,
                            show_y_title = TRUE,
                            color_db = "grey",
                            color_linear = "black",
                            color_carrier = "white",
                            color_threshold = "white",
                            color_bandwidth = "white",
                            show_lines = FALSE,
                            linewidth = 1
) {

  wl <- wave@samp.rate / as.numeric(freq_res)

  if (wl %% 2 == 1) {
    wl <- wl + 1
    }


  if (is.null(fmax) || fmax == 0) {
    fmax <- wave@samp.rate / 2 / 1000  # Convert to kHz
  }

  fmin <- as.numeric(fmin)
  fmax <- as.numeric(fmax)

  meanspec_data_dB <- seewave::meanspec(wave,
                                        f = wave@samp.rate,
                                        wl = wl,
                                        ovlp = ovlp,
                                        plot = FALSE,
                                        norm = TRUE,
                                        dB = "max0",
                                        wn = wn,
                                        FUN = fun,
                                        flim = c(fmin, fmax))

  meanspec_data_linear <- seewave::meanspec(wave,
                                            f = wave@samp.rate,
                                            wl = wl,
                                            ovlp = ovlp,
                                            plot = FALSE,
                                            norm = TRUE,
                                            wn = wn,
                                            FUN = fun,
                                            flim = c(fmin, fmax))

  meanspec_data <- data.frame(
    freq = meanspec_data_dB[, 1],
    mean_amp_dB = meanspec_data_dB[, 2],
    mean_amp_linear = meanspec_data_linear[, 2]
  )

  ########
  mean_amp_dB <- meanspec_data$mean_amp_dB
  # Check if mean_amp_dB has non-NA values
  if (all(is.na(mean_amp_dB)) || length(mean_amp_dB) == 0) {
    warning("mean_amp_dB contains only NA or is empty.")
    return(NULL)  # Or handle appropriately
  }

  min_amp <- min(mean_amp_dB, na.rm = TRUE)
  max_amp <- max(mean_amp_dB, na.rm = TRUE)

  # Avoid division by zero
  if (max_amp - min_amp == 0) {
    warning("max_amp and min_amp are equal; cannot normalize data.")
    # Handle the case appropriately, e.g., set norm_amp_dB to zeros or return NULL
    meanspec_data$norm_amp_dB <- rep(0, length(mean_amp_dB))
  } else {
    meanspec_data$norm_amp_dB <- (mean_amp_dB - min_amp) / (max_amp - min_amp)
  }

  minus20dB <- (-20 - min(meanspec_data$mean_amp_dB, na.rm = TRUE)) /
    (max(meanspec_data$mean_amp_dB, na.rm = TRUE) - min(meanspec_data$mean_amp_dB, na.rm = TRUE))

  carrier_freq <- meanspec_data$freq[which.max(meanspec_data$mean_amp_linear)]

  # Extract "Low" and "High" frequencies based on amplitude threshold
  if (total_bandwidth) {
    low_freq <- min(meanspec_data$freq[meanspec_data$freq < carrier_freq &
                                         meanspec_data$mean_amp_linear >= 0.1])
    high_freq <- max(meanspec_data$freq[meanspec_data$freq > carrier_freq &
                                          meanspec_data$mean_amp_linear >= 0.1])

  } else {

    low_freq_index <- max(which(meanspec_data$freq < carrier_freq &
                                  meanspec_data$mean_amp_linear <= 0.1))
    low_freq <- if (!is.na(low_freq_index) &&
                    low_freq_index < length(meanspec_data$freq)) meanspec_data$freq[low_freq_index + 1] else fmin

    high_freq_index <- min(which(meanspec_data$freq > carrier_freq &
                                   meanspec_data$mean_amp_linear <= 0.1))
    high_freq <- if (!is.na(high_freq_index) &&
                     high_freq_index > 1) meanspec_data$freq[high_freq_index - 1] else fmax
  }

  spec_linear <- seewave::spec(wave, PSD = TRUE, plot = FALSE)
  if (limit_indices) {

    spec_linear <- spec_linear[low_freq_index:high_freq_index, ]
  }
  spec.ent <- round(seewave::sh(spec_linear), 3)
  spec.flat <- round(seewave::sfm(spec_linear), 3)

  # Calculate spectral excursion
  freq_range_indices <- which(meanspec_data$freq >= low_freq & meanspec_data$freq <= high_freq)
  spec.excursion <- round(sum(abs(diff(meanspec_data$mean_amp_linear[freq_range_indices]))), 3)
  spec.sd <- round(sd(meanspec_data$mean_amp_linear[freq_range_indices]), 3)
  spec.var <- round(var(meanspec_data$mean_amp_linear[freq_range_indices]), 3)

  # Summary Statistics
  summary_df <- tibble(
    spec.ex = spec.excursion,
    spec.sd = spec.sd,
    spec.var = spec.var,
    spec.ent = spec.ent,
    spec.flat = spec.flat,
    carrier = round(carrier_freq, 2),
    low.f = round(low_freq, 2),
    high.f = round(high_freq, 2)
  )

  summary_df <- summary_df |>
    mutate(bandw = round(high.f - low.f, 2))


  if (italic_title) {
    plot_title <- paste0("<i>", plot_title, "</i>")
  }


  if (db_shade) {
    spectrum_plot <- plot_ly(x = ~meanspec_data$freq) |>
      add_ribbons(ymin = 0, ymax = ~meanspec_data$norm_amp_dB,
                  fillcolor = color_db,
                  line = list(color = color_db),
                  opacity = 0.7,
                  name = "Scaled dB",
                  hoverinfo = "x+y",
                  hovertemplate = "<b>Frequency:</b> %{x:.1f}
                  kHz<br><b>Amplitude:</b>%{y:.3f}<br>") |>
      add_ribbons(ymin = 0, ymax = ~meanspec_data$mean_amp_linear,
                  fillcolor = color_linear,
                  line = list(color = color_linear),
                  opacity = 0.7,
                  name = "Scaled Linear",
                  hoverinfo = "x+y",
                  hovertemplate = "<b>Frequency:</b> %{x:.1f}
                  kHz<br><b>Amplitude:</b>%{y:.1f}<br>") |>
      layout(
        title = list(text = plot_title),
        xaxis = list(title = if (show_x_title) "Frequency (kHz)" else "",
                     range = c(fmin, fmax),
                     tickvals = seq(fmin, fmax, length.out = x_breaks),
                     tickformat = ".0f"),
        yaxis = list(title = if (show_y_title) "Relative Amplitude" else "",
                     range = c(0, 1),
                     tickvals = seq(0, 1, by = 0.2)),
        showlegend = FALSE
      )

  } else {
    spectrum_plot <- plot_ly(x = ~meanspec_data$freq) |>
      add_ribbons(ymin = 0, ymax = ~meanspec_data$mean_amp_linear,
                  fillcolor = color_linear,
                  line = list(color = color_linear),
                  opacity = 0.9,
                  name = "Linear") |>
      layout(
        title = list(text = plot_title),
        xaxis = list(title = if (show_x_title) "Frequency (kHz)" else "",                     range = c(fmin, fmax),
                     tickvals = seq(fmin, fmax, length.out = x_breaks),
                     tickformat = ".0f"),
        yaxis = list(title = if (show_y_title) "Amplitude" else "",
                     range = c(0, 1),
                     tickvals = seq(0, 1, by = 0.2)),
        showlegend = FALSE
      )

  }

  if (show_lines) {
    spectrum_plot <- spectrum_plot |>
      add_segments(x = carrier_freq, xend = carrier_freq, y = 0, yend = 1,
                   line = list(color = color_carrier, width = linewidth),
                   name = "Carrier/Peak")

    if (db_shade) {
      spectrum_plot <- spectrum_plot |>
        add_segments(x = low_freq, xend = high_freq, y = minus20dB, yend = minus20dB,
                     line = list(color = color_threshold, dash = "dash", width = linewidth),
                     name = "dB Threshold")
    }

    spectrum_plot <- spectrum_plot |>
      add_segments(x = low_freq, xend = high_freq, y = 0.1, yend = 0.1,
                   line = list(color = color_threshold, dash = "dash", width = linewidth),
                   name = "Linear Threshold") |>
      add_segments(x = low_freq, xend = low_freq, y = 0, yend = 1,
                   line = list(color = color_bandwidth, width = linewidth),
                   name = "Low Frequency") |>
      add_segments(x = high_freq, xend = high_freq, y = 0, yend = 1,
                   line = list(color = color_bandwidth, width = linewidth),
                   name = "High Frequency")
  }


  if (add_params) {
    params_text1 <- paste0("Sampling Rate: ", wave@samp.rate / 1000, " kHz",
                           "\nResolution: ", freq_res, " Hz/bin",
                           "\nWindow Size: ", wl,
                           "\nSummary Function: ", fun,
                           "\nFilter Function: ", wn,
                           "\nWindow Overlap: ", ovlp, "%",
                           "\nTotal Bandwidth: ", total_bandwidth,
                           "\nIndex Limits: ", limit_indices)

    spectrum_plot <- spectrum_plot |>
      add_annotations(
        x = 1, y = 1, text = params_text1, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "right", yanchor = "top", font = list(size = 12),
        align = "left", bgcolor = "white", opacity = 0.8
      )
  }

  if (add_summary) {
    # Summary Statistics annotations
    measurements <-
      paste0("Excursion: ", summary_df$spec.ex,
             "\nStd: ", summary_df$spec.sd,
             "\nVariance: ", summary_df$spec.var,
             "\nEntropy: ", summary_df$spec.ent,
             "\nFlatness: ", summary_df$spec.flat,
             "\nCarrier: ", summary_df$carrier, " kHz",
             "\nHigh F.: ", summary_df$low.f, " kHz",
             "\nLow F.: ", summary_df$high.f, " kHz",
             "\nBandwidth.: ", summary_df$bandw, " kHz"

      )

    spectrum_plot <- spectrum_plot |>
      add_annotations(
        x = 0.01, y = 1, text = measurements, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "left", yanchor = "top", font = list(size = 12),
        align = "left", bgcolor = "white", opacity = 0.8
      )
  }

  return(list(plot = spectrum_plot, summary = summary_df))
}
