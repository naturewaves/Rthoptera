#' Power Spectrum with ggplot2
#'
#' This function generates a power spectrum plot using `ggplot2` based on the
#' provided waveform object (`wave`). The plot can display normalized amplitude
#' values on both dB and linear scales, and includes options for customizing the
#' display of the carrier frequency, bandwidth, and other spectral details.
#'
#' @param wave A Wave object (from `tuneR` package), representing the sound
#' waveform.
#' @param auto_wl Logical. If `TRUE`, the window length (`wl`) is automatically
#' calculated based on the sampling rate of the waveform.
#' @param wl Numeric. Window length for the FFT. Ignored if `auto_wl` is `TRUE`.
#' @param ovlp Numeric. Overlap between windows in percentage (0 to 100).
#' Default is 75.
#' @param ymin Numeric. Minimum value for the y-axis (used when plotting
#' in dB scale).
#' @param y_breaks Numeric vector. Break points for the y-axis in the dB
#' scale plot.
#' @param x_breaks Integer. Number of breaks for the x-axis (frequency).
#' @param y_position Character. Position of the y-axis: "left" or "right".
#' @param x_position Character. Position of the x-axis: "bottom" or "top".
#' @param flip Logical. If `TRUE`, flips the x and y axes.
#' @param color_db Character. Color for the dB scale plot.
#' @param color_linear Character. Color for the linear scale plot.
#' @param color_carrier Character. Color for the carrier frequency line.
#' @param color_threshold Character. Color for the threshold line.
#' @param color_bandwidth Character. Color for the bandwidth lines.
#' @param fun Character. Summary function for calculating the power spectrum,
#' default is "mean".
#' @param wn Character. Windowing function for the FFT. Default is "blackman".
#' @param show_x_title Logical. If `TRUE`, displays the x-axis title.
#' @param show_y_title Logical. If `TRUE`, displays the y-axis title.
#' @param add_params Logical. If `TRUE`, adds parameter information
#' (window size, overlap, etc.) to the plot.
#' @param add_summary Logical. If `TRUE`, adds spectral summary statistics
#' (entropy, flatness, carrier frequency, bandwidth) to the plot.
#' @param plot_title Character. Title for the plot.
#' @param italic_title Logical. If `TRUE`, italicizes the plot title.
#' @param fmin Numeric. Minimum frequency to display (in kHz).
#' @param fmax Numeric. Maximum frequency to display (in kHz). If `NULL`,
#' defaults to half the sampling rate (Nyquist frequency).
#' @param linewidth Numeric. Line width for the carrier and bandwidth lines.
#' @param total_bandwidth Logical. If `TRUE`, calculates the total bandwidth
#' using a threshold of 0.1.
#' @param show_lines Logical. If `TRUE`, displays lines for the carrier
#' frequency and bandwidth.
#'
#' @return A `ggplot` object for the power spectrum plot.
#'
#' @import ggplot2
#' @import seewave
#' @importFrom tuneR Wave

#' @export
#'
#' @examples
#' \dontrun{
#' spectrum_ggplot(coryphoda)
#' }
spectrum_ggplot <- function(wave,
                            auto_wl = TRUE,
                            wl = NULL,
                            ovlp = 75,
                            ymin = -100, # use only when plotting in dB
                            y_breaks = c(-40, -20, 0), # use only for dB
                            x_breaks = 6,
                            y_position = "left",
                            x_position = "bottom",
                            flip = FALSE,
                            color_db = "#4d4d4d", # Grey
                            color_linear = "#000000", # Black
                            color_carrier = "#1e90ff", # Dodger Blue
                            color_threshold = "#228b22", # Forest Green
                            color_bandwidth = "#228b22", # Forest Green
                            fun = "mean",
                            wn = "blackman",
                            show_x_title = TRUE,
                            show_y_title = TRUE,
                            add_params = FALSE,
                            add_summary = TRUE,
                            plot_title = "",
                            italic_title = FALSE,
                            fmin = 0,
                            fmax = NULL,
                            linewidth = 1,
                            total_bandwidth = FALSE,
                            show_lines = FALSE) {
  if (auto_wl) {
    # Standardized window length according to sample rate
    wl <- round(wave@samp.rate * 1e-2)

    if (wl %% 2 != 0) {
      wl <- wl + 1
    }
  } else {
    wl <- wl
  }

  if (wl %% 2 != 0) {
    wl <- wl + 1
  }

  # Automatically set fmax to Nyquist frequency if not provided
  if (is.null(fmax) || fmax == 0) {
    fmax <- wave@samp.rate / 2 / 1000 # Convert to kHz
  }

  # Ensure fmin and fmax are numeric
  fmin <- as.numeric(fmin)
  fmax <- as.numeric(fmax)

  # Calculate the dB scale spectrum
  meanspec_data_dB <- seewave::meanspec(wave,
    f = wave@samp.rate,
    wl = wl,
    ovlp = ovlp,
    plot = FALSE,
    norm = TRUE, # Normalize to max amplitude for dB scale
    dB = "max0",
    wn = wn,
    FUN = fun,
    flim = c(fmin, fmax)
  )

  # Calculate the linear scale spectrum
  meanspec_data_linear <- seewave::meanspec(wave,
    f = wave@samp.rate,
    wl = wl,
    ovlp = ovlp,
    plot = FALSE,
    norm = TRUE, # Normalize to max amplitude for linear scale
    wn = wn,
    FUN = fun,
    flim = c(fmin, fmax)
  )

  # Convert data to data frames
  meanspec_data <- data.frame(
    freq = meanspec_data_dB[, 1],
    mean_amp_dB = meanspec_data_dB[, 2],
    mean_amp_linear = meanspec_data_linear[, 2]
  )

  # Min-max normalization for both dB and linear scales
  meanspec_data$norm_amp_dB <- (meanspec_data$mean_amp_dB - min(meanspec_data$mean_amp_dB)) /
    (max(meanspec_data$mean_amp_dB) - min(meanspec_data$mean_amp_dB))

  meanspec_data$norm_amp_linear <- (meanspec_data$mean_amp_linear -
                                      min(meanspec_data$mean_amp_linear)) /
    (max(meanspec_data$mean_amp_linear) - min(meanspec_data$mean_amp_linear))

  # Carrier frequency (maximum amplitude frequency)
  carrier_freq <- meanspec_data$freq[which.max(meanspec_data$mean_amp_dB)]

  # Calculate low and high frequencies based on the 0.1 threshold
  if (total_bandwidth) {
    # Find the last frequencies below the threshold, ignoring gaps
    low_freq <- min(
      meanspec_data$freq[meanspec_data$freq < carrier_freq & meanspec_data$norm_amp_linear >= 0.1]
    )
    high_freq <- max(
      meanspec_data$freq[meanspec_data$freq > carrier_freq & meanspec_data$norm_amp_linear >= 0.1]
    )
  } else {
    # Regular bandwidth: Find the first crossing below the threshold, then
    # select the next valid point. For low frequency, search leftward
    # from the carrier
    low_freq_index <- max(which(
      meanspec_data$freq < carrier_freq & meanspec_data$norm_amp_linear <= 0.1
    ))
    if (!is.na(low_freq_index) && low_freq_index < length(meanspec_data$freq)) {
      low_freq <- meanspec_data$freq[low_freq_index + 1]
    } else {
      low_freq <- fmin # Default to fmin if no suitable frequency is found
    }

    # For high frequency, search rightward from the carrier
    high_freq_index <- min(which(
      meanspec_data$freq > carrier_freq & meanspec_data$norm_amp_linear <= 0.1
    ))
    if (!is.na(high_freq_index) && high_freq_index > 1) {
      high_freq <- meanspec_data$freq[high_freq_index - 1]
    } else {
      high_freq <- fmax # Default to fmax if no suitable frequency is found
    }
  }


  # Calculate the spectral indices within the frequency range of interest
  spec_linear <- seewave::spec(
    wave,
    PSD = TRUE, plot = FALSE, flim = c(low_freq, high_freq)
  )
  # Spectral entropy:
  spec.ent <- seewave::sh(spec_linear)
  # Spectral flatness
  spec.flat <- seewave::sfm(spec_linear)

  # Create the plot with normalized scales
  spectrum_plot <- ggplot(meanspec_data, aes(x = freq)) +
    theme_minimal(base_size = 15)

  # Primary y-axis with normalized dB scale
  spectrum_plot <- spectrum_plot +
    geom_ribbon(aes(ymin = 0, ymax = norm_amp_dB),
      fill = color_db, alpha = 0.9
    ) +
    geom_ribbon(aes(ymin = 0, ymax = norm_amp_linear),
      fill = color_linear, alpha = 0.9
    ) +
    scale_y_continuous(
      name = "Normalized Amplitude",
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      expand = expansion(mult = c(0, .1)),
      position = y_position
    )

  # Set x-axis parameters
  spectrum_plot <- spectrum_plot +
    scale_x_continuous(
      limits = c(fmin, fmax), # Explicitly set x-axis limits
      expand = c(0, 0),
      position = x_position,
      breaks = scales::breaks_pretty(n = x_breaks),
      labels = scales::label_number(zero.print = "")
    ) +
    theme_bw() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.ticks.y = element_line(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      legend.position = "none",
      plot.title = element_text(face = if (italic_title) "italic" else "plain")
    ) +
    labs(
      x = if (show_x_title) "Frequency (kHz)" else NULL,
      title = plot_title
    )

  if (show_lines) {
    spectrum_plot <- spectrum_plot +
      geom_vline(
        xintercept = carrier_freq, color = color_carrier,
        linetype = "solid", linewidth = linewidth
      ) +
      annotate("segment",
        x = low_freq, xend = high_freq, y = 0.1, yend = 0.1,
        color = color_threshold,
        linetype = "dashed", linewidth = linewidth
      ) +
      geom_vline(
        xintercept = low_freq, color = color_bandwidth,
        linetype = "solid", linewidth = linewidth
      ) +
      geom_vline(
        xintercept = high_freq, color = color_bandwidth,
        linetype = "solid", linewidth = linewidth
      )
  }

  if (add_params) {
    if (auto_wl) {
      winsize <- paste0("Win. size: ", wl, " (auto)")
    } else {
      winsize <- paste0("Win. size: ", wl)
    }
    # Parameters annotation
    params_text1 <-
      paste0(
        winsize,
        "\nLeak. function: ", wn,
        "\nSumm. Function: ", fun,
        "\nWin. Overlap:", ovlp, "%"
      )

    spectrum_plot <- spectrum_plot +
      annotate("label",
        x = Inf,
        y = Inf,
        hjust = 1.05,
        vjust = 1.05,
        label = params_text1,
        size = 4,
        color = "black",
        fill = "white",
        alpha = 0.8
      )
  }

  if (add_summary) {
    # Summary Statistics annotations
    measurements <-
      paste0(
        "Entropy: ", round(spec.ent, 2),
        "\nFlatness: ", round(spec.flat, 2),
        "\nPeak F: ", round(carrier_freq, 2), " kHz",
        "\nHigh F.: ", round(high_freq, 2), " kHz",
        "\nLow F.: ", round(low_freq, 2), " kHz"
      )

    spectrum_plot <- spectrum_plot +
      annotate("label",
        x = -Inf,
        y = Inf,
        hjust = -0.05,
        vjust = 1.05,
        label = measurements,
        size = 4,
        color = "black",
        fill = "white",
        alpha = 0.8
      )
  }

  if (flip) {
    spectrum_plot <- spectrum_plot +
      coord_flip()
  }

  return(spectrum_plot)
}
