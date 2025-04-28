#' Plot a representative spectrum of a population (or subspecies, species, etc.)
#'
#' This function generates an averaged Power Spectral Density (PSD) plot multiple
#' input Waves (e.g., population, species, etc,). The line is soothed with local
#' regression (LOESS).
#'
#' @param wave An object of class `Wave` containing the audio data to be
#' analyzed.
#' @param binomial Character. The binomial name of the species. Empty by default.
#' @param freq_res Numeric. The frequency resolution to be used for the
#' frequency spectrum analysis. Use this argument consistently for standardized
#' measurements across recordings with different sampling rate. Default is 10 Hz
#' per frequency bin.
#' @param s_rate_out Sample rate of the output summary PSD, in kHz.
#' @param fun Character. Select a function to compute each power spectrum. Options
#' are "mean", "min", "max".
#' @param wn  Character. Select a window function to for the FFT. Options are
#' "hanning" (Default), "hamming", bartlett", "blackaman", "flattop", and "rectangular".
#' @param flim Numeric list of length 2. Range of the frequency axis (KhZ).
#' @param loess_span The span of the LOESS smoothing, as a proportion. Higher
#' values result in smoother lines. Default is 0.05.
#' @param line_color Character. Color for the line representing the mean PSD.
#' @param line_width Numeric. Width of the mean line.
#' @param sd_color Character. Color for the standard deviation ribbon.
#' @param sd_alpha Numeric. Transparency level for the SD ribbon. Default is 0.3 (30%).
#' @param title_x Character. Title for the X-axis. Default is "Frequency (kHz)".
#' @param title_y Character. Title for the Y-axis. Empty by default.
#' @param show_x_labs Logical. If TRUE, the labels (i.e., numbers) are shown in
#' the X-axis. Defaults to TRUE.
#' @param show_y_labs Logical. If TRUE, the labels (i.e., numbers) are shown in
#' the Y-axis. Defaults to FALSE
#' @param show_n Logical. If TRUE, the number of samples (i.e., recordings) is
#' shown as "n=" on the top-right of the plot.
#' @param save Logical. Whether to save the plot immediately.
#' @param width Numeric. Relative width of the output plot. Default is 960.
#' @param height Numeric. Relative height of the output plot. Default is 540.
#' @param dpi Numeric. Resolution in dots per inch. Default is 200.
#' @return An averaged and smoothed power spectral density plot representing all
#' the Waves in the list provided to the `waves` argument. The plot shows the
#' mean and confidence interval (95% by default) along with the peak frequency,
#' the scientific name of the species as provided in the `binomial` argument,
#' and the number of samples (Wave objects) from which the mean was calculated.
#' @export
#' @import fftw
#' @importFrom tibble tibble
#' @importFrom seewave meanspec
#' @import ggplot2
#' @examples
#'  \dontrun{
#' library(RthopteraSounds)
#' data(coryphoda)
#' data(tettigonia)
#' # Assume the two Waves are two individuals from the same species
#' waves_list <- c(coryphoda, tettigonia)
#' population_spectrum(waves = waves_list, freq_res = 50, show_x_labs = TRUE,
#' title_x = "", show_n = TRUE, binomial = "Coryphoda albidicollis")
#' }
spectrum_pop <- function(waves,
                         binomial = NULL,
                         freq_res = 400,
                         s_rate_out = 96000,
                         fun = "max",
                         wn = "hanning",
                         flim = NULL,
                         loess_span = 0.1,
                         line_color = "green4",
                         line_width = 1,
                         sd_color = "dodgerblue",
                         sd_alpha = 0.5,
                         title_x = "Frequency (kHz)",
                         title_y = "",
                         show_x_labs = TRUE,
                         show_y_labs = FALSE,
                         show_n = TRUE,
                         save_to = NULL,
                         save = FALSE,
                         width = 960,
                         height = 540,
                         dpi = 200
                         ) {




  # Get all sampling rates
  s_rates <- sapply(waves, function(w) w@samp.rate)

  # Resample waves that don't match target rate
  waves <- lapply(waves, function(w) {
    if (w@samp.rate != s_rate_out) {
      message(paste0("Resampling from ", w@samp.rate, " to ", s_rate_out, " Hz"))

      # Convert to numeric to prevent integer overflow
      w@left <- as.numeric(w@left)

      # Calculate resampling factor as numeric
      resample_factor <- s_rate_out/w@samp.rate

      # Use signal package's resample which handles large rates better
      resampled_left <- signal::resample(w@left, p = round(resample_factor * 100), q = 100)

      # Create new Wave object
      return(Wave(left = as.integer(resampled_left),
                  samp.rate = s_rate_out,
                  bit = w@bit))
    } else {
      return(w)
    }
  })



  spectra_list <- lapply(waves, function(wave) {
    wl = wave@samp.rate/freq_res
    if (wl %% 2 == 1) wl <- wl + 1

    spec_data <- seewave::meanspec(wave,
                                   PSD = TRUE,
                                   f = wave@samp.rate,
                                   wl = wl,
                                   plot = FALSE,
                                   norm = TRUE,
                                   dB = NULL,
                                   wn = wn,
                                   FUN = fun)

    data.frame(frequency = spec_data[,1], amplitude = spec_data[,2])
  })


  combined_spectra <- bind_rows(spectra_list, .id = "wave_id")

  # Calculate mean and SD for each frequency bin
  summary_spectra <- combined_spectra |>
    group_by(frequency) |>
    summarise(
      mean_amplitude = mean(amplitude, na.rm = TRUE),
      sd_amplitude = sd(amplitude, na.rm = TRUE)
    ) |>
    mutate(
      # Normalize both mean and SD by the maximum mean amplitude
      norm_factor = max(mean_amplitude, na.rm = TRUE),
      mean_amplitude = mean_amplitude / norm_factor,
      sd_amplitude = sd_amplitude / norm_factor
    )

  # Calculate dynamic y-axis limits based on SD range
  y_max <- max(summary_spectra$mean_amplitude + summary_spectra$sd_amplitude, na.rm = TRUE)
  y_min <- min(summary_spectra$mean_amplitude - summary_spectra$sd_amplitude, na.rm = TRUE)

  # Add small buffer (5% of range) to y-axis limits
  y_buffer <- 0.05 * (y_max - y_min)
  y_limits <- c(y_min - y_buffer, y_max + y_buffer)

  # Ensure y-axis doesn't go below 0
  y_limits[1] <- max(0, y_limits[1])


  peak_freq <- summary_spectra$frequency[which.max(summary_spectra$mean_amplitude)]
  peak_amplitude <- max(summary_spectra$mean_amplitude)

  # Modified plot creation with dynamic y-axis limits
  p <- ggplot(summary_spectra, aes(x = frequency, y = mean_amplitude)) +
    geom_ribbon(aes(ymin = pmax(mean_amplitude - sd_amplitude, 0),
                    ymax = mean_amplitude + sd_amplitude),
                fill = sd_color, alpha = sd_alpha
    ) +
    geom_line(color = line_color, linewidth = line_width) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    labs(x = title_x, y = title_y) +
    theme_classic() +
    theme(
      axis.line = element_line(linewidth = 1),
      axis.ticks.x = element_line(linewidth = 0.8),
      axis.ticks.length.x.bottom = unit(0.3, "cm"),
      axis.ticks.y = element_line(linewidth = 0.8),
      axis.ticks.length.y.left = unit(0.3, "cm"),
      legend.position = "none"
    )

  if (!is.null(flim)) p <- p + coord_cartesian(xlim = flim)
  if (!show_x_labs) p <- p + theme(axis.text.x = element_blank())
  if (!show_y_labs) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  # Add annotations
  p <- p +
    annotate("point", x = peak_freq, y = peak_amplitude + 0.05,
             shape = 25, size = 4, color = "red", fill = "red") +
    annotate("text", x = peak_freq+3, y = peak_amplitude+0.01,
             label = paste(round(peak_freq, 1), "kHz"), color = "black",
             size = 5)

  if (show_n) {
    p <- p + annotate("text", x = Inf, y = Inf,
                      label = paste0("italic(n) == ", length(waves)),
                      hjust = 1.1, vjust = 1.1, parse = TRUE, size = 5)
  }

  if (!is.null(binomial)) {
    p <- p + labs(title = binomial) + theme(plot.title = element_text(face = "italic"))
  }

  if (save) {
    filename <- if (!is.null(binomial)) {
      file.path(save_to %||% getwd(), paste0(gsub(" ", "_", binomial), "_spectrum_pop.png"))
    } else {
      file.path(save_to %||% getwd(), "spectrum_pop.png")
    }
    ggsave(filename, plot = p, width = width/dpi, height = height/dpi, dpi = dpi)
  }

  return(p)
}

