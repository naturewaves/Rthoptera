#' Plot a representative spectrum of a population (or subspecies, species, etc.)
#'
#' This function generates a Power Spectral Density (PSD) plot from
#' the average of multiple input Waves (e.g., population, species, etc,). The
#' line is soothed with local regression (LOESS).
#'
#' @param wave An object of class `Wave` containing the audio data to be
#' analyzed.
#' @param freq_res Numeric. The frequency resolution to be used for the
#' frequency spectrum analysis. Use this argument consistently for standardized
#' measurements across recordings with different sampling rate. Default is 10 Hz
#' per frequency bin.
#' @param loess_span The span of the LOESS smoothing, as a proportion. Higher
#' values result in smoother lines. Default is 0.05.
#' @param conf_level Numeric. Confidence interval level. Default is 0.95 (95%).
#' @param mean_col Character. Color for the line representing the mean PSD.
#' @param line_width Numeric. Width of the mean line.
#' @param ci_col Character. Color for the confidence interval ribbon.
#' @param ci_alpha Numeric. Transparency level for the confidence interval
#' ribbon. Default is 0.7 (70%).
#' @param title_y Character. Title for the Y-axis. Empty by default.
#' @param title_x Character. Title for the X-axis. Default is "Frequency (kHz)".
#' @param show_x_labs Logical. If TRUE, the labels (i.e., numbers) are shown in
#' the X-axis. Defaults to TRUE.
#' @param show_n Logical. If TRUE, the number of samples (i.e., recordings) is
#' shown as "n=" on the top-right of the plot.
#' @param binomial Character. The binomial name of the species. Empty by default.
#' @param ... Other arguments passed to meanspec() from the seewave package.
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
                         freq_res = 300,
                         fun = "mean",
                         wn = "hanning",
                         loess_span = 0.1,
                         conf_level = 0.95,
                         color = "green4",
                         mean_width = 1,
                         ci_alpha = 0.3,
                         title_y = "",
                         title_x = "Frequency (kHz)",
                         show_x_labs = TRUE,
                         show_n = TRUE,
                         binomial = NULL,
                         save_to = NULL,
                         save = FALSE,
                         width = 960,
                         height = 540,
                         dpi = 200,
                         ...) {

  spectra_list <- lapply(waves, function(wave) {

    wl = wave@samp.rate/freq_res

    if (wl %% 2 == 1) {
      wl <- wl + 1
    }

    amp_max <- switch(as.character(wave@bit),
                      "16" = 32768,
                      "24" = 8388607,
                      "32" = 2147483647,
                      stop("Unsupported bit depth: ", wave@bit))

    # Suppress messages from meanspec()
    meanspec_data <- seewave::meanspec(wave,
                                       f = wave@samp.rate,
                                       wl = wl,
                                       ovlp = 50,
                                       plot = FALSE,
                                       norm = TRUE,
                                       dB = "max0",
                                       wn = wn,
                                       FUN = fun
    )

    # Convert matrix to data frame
    meanspec_df <- as.data.frame(meanspec_data)
    colnames(meanspec_df) <- c("frequency", "amplitude")

    # Normalize the amplitude to dB scale
    # meanspec_df$amplitude <- 20 * log10(abs(meanspec_df$amplitude) / amp_max)

    # Return the processed spectrum as a data frame
    meanspec_df
  })

  # Combine all spectra into a single data frame
  combined_spectra <- bind_rows(spectra_list, .id = "wave_id")

  # Calculate mean and confidence interval for each frequency bin
  summary_spectra <- combined_spectra %>%
    group_by(frequency) %>%
    summarise(
      mean_amplitude = mean(amplitude, na.rm = TRUE)
    )

  # Find the peak frequency
  peak_freq <- summary_spectra$frequency[which.max(summary_spectra$mean_amplitude)]
  peak_amplitude <- max(summary_spectra$mean_amplitude)

  # Plot the mean spectrum with confidence intervals
  p <- ggplot(summary_spectra, aes(x = frequency, y = mean_amplitude)) +
    geom_line(color = "transparent") +
    geom_smooth(
      method = "loess",
      span = loess_span,
      color = color,
      linewidth = mean_width,
      se = TRUE,
      level = conf_level,
      fill = color,
      alpha = ci_alpha
    ) +
    # Add inverted red triangle at peak frequency (always above the mean line)
    annotate(
      "point",
      x = peak_freq, y = peak_amplitude + 3, # Slightly above the peak
      shape = 25, size = 4, color = "red", fill = "red"
    ) +
    # Add red number below the triangle, near the zero in Y
    annotate(
      "text",
      x = peak_freq, y = peak_amplitude - 10,
      label = round(peak_freq, 1),
      color = "red", size = 5, vjust = 1
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x = title_x,
      y = title_y
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(linewidth = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(linewidth = 0.8),
      axis.ticks.length.x.bottom = unit(0.3, "cm")
    )

  # Hide x-axis labels if show_x_labs is FALSE
  if (!show_x_labs) {
    p <- p + theme(axis.text.x = element_blank())
  }

  # Add "n = <number of waves>" in the top-right corner if show_n is TRUE
  if (show_n) {
    n_waves <- length(waves)
    p <- p + annotate(
      "text",
      x = Inf, y = Inf,
      label = paste0("italic(n) == ", n_waves),
      hjust = 1.1, vjust = 1,
      parse = TRUE,
      size = 5,
      color = "black"
    )
  }

  # Add binomial annotation in the top-left corner if binomial is provided
  if (!is.null(binomial)) {
    p <- p +
      labs(title = binomial) +
      theme(plot.title = element_text(face = "italic"))
  }

  # Save the plot if save is TRUE and save_to is provided
  if (save) {

    if (is.null(save_to)) {
      save_to <- getwd()
    }

    if (!is.null(binomial)) {
      filename <- file.path(save_to, paste0(gsub(" ", "_", binomial), "_spectrum_pop.png"))
    } else {
      filename <- file.path(save_to, "spectrum_pop.png")
    }
    ggsave(filename, plot = p, width = width / dpi, height = height / dpi, dpi = dpi, ...)
  }

  return(p)
}
