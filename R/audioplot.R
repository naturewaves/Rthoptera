#' Fast Spectrogram with Derived Pseudo-Oscillogram and Power Spectrum
#'
#' @param wave A Wave object created with tuneR package (e.g., readWave).
#' @param fft Numeric. Size of FFT window (must be power of 2). Default is
#' "auto", which automatically calculates a proper length based on 'enhance'.
#' @param enhance Character. Either 'freq' to prioritize frequency
#' resolution or 'time' to prioritize temporal resolution when fft="auto".
#' @param overlap Numeric. Overlap between windows (0 to 95%)
#' @param window Character. Type of window function. Default is "hanning".
#' Also accepts "hamming" and "blackman".
#' @param contrast Numeric. Dynamic range for visualization (0-100%).
#' @param flim Numeric vector of length 2. Frequency limits, in kHz.
#' @param tlim Numeric vector of length 2. Time limits, in seconds.
#' @param zero.pad Numeric. Zero-padding factor.
#' @param output.file Character. Optional file path to save the spectrogram.
#' @param plot.title Character. Plot title
#' @param use.ragg Logical. Whether to use RAGG for GPU-accelerated rendering.
#' @param palette Character. Color palette for spectrogram. Can be a
#' character string (palette name from \code{hcl.colors}) or a function
#' that returns a color vector.
#' @param palette_rev Logical. Whether to reverse the color palette. To obtain
#' a white background with dark signals, set to TRUE and use palette="Grays".
#' @param osc.show Logical. Whether to show the pseudo-oscillogram
#' @param osc.fun Character. Name of the summary statistic used to create
#' the pseudo-oscillogram (e.g., "mean", "max", "sd", etc.)
#' @param osc.color Character. Color for the pseudo-oscillogram
#' @param osc.height Numeric. Relative height of oscillogram (0-1)
#' @param spec.show Logical. Whether to show the power spectrum
#' @param spec.fun Character. Summary function to apply across frequency
#' bins for power spectrum calculation (e.g., "mean", "max", "sd", etc.).
#' @param spec.color Characer. Color for the power spectrum.
#' @param spec.width Numeric. Relative width of power spectrum (0-1).
#' @param spec.linear Logical. Whether to show the power spectrum in linear scale. Default
#' is TRUE.
#'
#' @importFrom fields image.plot
#' @importFrom fftw planFFT FFT
#' @importFrom ragg agg_png
#'
#' @export
#'
#' @details
#' When fft is set to 'auto' (recommended), the function takes the 'enhance'
#' argument to decide whether to enhance spectral or temporal resolution. We
#' recommend "enhance= 'freq'" for high-quality factor (i.e., 'tonal') sounds
#' such as those produced by most crickets, and "enhance='time" for low-quality
#' factor (i.e., 'raspy') sounds such as those produced by katydids and cicadas.
#'
#' @examples
#' \dontrun{
#' library(RthopteraSounds)
#' data("coryphoda")
#' data("gryllus")
#' audioplot(coryphoda, enhance = "time",
#' osc.show = FALSE, palette = 'Zissou 1')
#' audioplot(coryphoda, enhance = 'time', osc.show = FALSE,
#' spec.linear = TRUE) # great
#' audioplot(coryphoda, enhance = "time")
#' audioplot(gryllus, enhance = "freq")
#' audioplot(gryllus, enhance = "time")
#'
#' # Grayscale with white background
#' audioplot(coryphoda, palette = "Grays", palette_rev = TRUE)
#' }
#' @return Invisible list containing spectrogram data
audioplot <- function(wave,
                      fft = "auto",
                      enhance = "time",
                      overlap = 75,
                      window = "hanning",
                      contrast = 50,
                      flim = NULL,
                      tlim = NULL,
                      zero.pad = 8,
                      output.file = NULL,
                      plot.title = "",
                      use.ragg = TRUE,
                      palette = "Mako",
                      palette_rev = FALSE,
                      osc.show = TRUE,
                      osc.fun = "mean",
                      osc.color = "black",
                      osc.height = 0.25,
                      spec.show = TRUE,
                      spec.fun = "mean",
                      spec.color = "black",
                      spec.width = 0.15,
                      spec.linear = TRUE
) {

  # Ensure graphics state is always reset on exit or error
  on.exit({
    # Reset layout to default
    tryCatch(graphics::layout(1), error = function(e) NULL)
    # Reset par to defaults
    tryCatch(par(mar = c(5, 4, 4, 2) + 0.1), error = function(e) NULL)
  }, add = TRUE)



  if (enhance == "freq"){
    q.factor = "h"
  } else if (enhance == "time"){
    q.factor = "l"
  } else {
    stop("'enhance' must be 'freq' or 'time'.")
  }


  dur <- length(wave@left) / wave@samp.rate

  nyq <- (wave@samp.rate/2)/1000

  if (is.null(flim)) {
    flim = c(0, nyq)
  }

  if (is.null(tlim)) {
    tlim = c(0, dur)
  }

  if (fft == "auto"){

    if (q.factor == "l"){
      fft <- round(wave@samp.rate * sqrt(dur) * 20e-4)
    } else if (q.factor == "h"){
      fft <- round(wave@samp.rate * sqrt(dur) * 5.224e-3)
    } else {
      stop("q.factor should be either 'lq' (e.g., for katydids) or 'hq' (e.g., for crickets.")
    }



    if (fft %% 2 != 0) {

      fft <- fft + 1

    }

  }


  dynamic.range <- 100 - contrast

  signal <- wave@left
  fs <- wave@samp.rate

  # Normalize signal
  signal <- signal / max(abs(signal))

  # Calculate window function
  window <- switch(window,
                   "hanning" = 0.5 * (1 - cos(2 * pi * seq(0, 1, length.out = fft))),
                   "hamming" = 0.54 - 0.46 * cos(2 * pi * seq(0, 1, length.out = fft)),
                   "blackman" = 0.42 - 0.5 * cos(2 * pi * seq(0, 1, length.out = fft)) +
                     0.08 * cos(4 * pi * seq(0, 1, length.out = fft)),
                   rep(1, fft))  # Default rectangular window


  ovlp <- overlap * 0.01
  # Calculate step size based on overlap
  step_size <- round(fft * (1 - ovlp))

  # Calculate actual FFT size (with zero padding)
  fft_size <- fft * zero.pad

  # Pre-allocate matrix for spectrogram (rows based on padded FFT)
  num_segments <- floor((length(signal) - fft) / step_size) + 1
  spectrogram <- matrix(0, nrow = fft_size/2 + 1, ncol = num_segments)

  # Pre-plan FFT for maximum performance (using the padded size)
  plan <- planFFT(fft_size)

  # Process each segment
  for (i in 1:num_segments) {
    start_idx <- (i - 1) * step_size + 1
    end_idx <- start_idx + fft - 1

    if (end_idx > length(signal)) break

    # Extract segment and apply window
    segment <- signal[start_idx:end_idx] * window

    # Apply zero-padding if needed
    if (zero.pad > 1) {
      segment <- c(segment, rep(0, fft * (zero.pad - 1)))
    }

    # Perform FFT using FFTW (on padded segment)
    fft_result <- FFT(segment, plan = plan)

    # Calculate magnitude and convert to dB (use padded FFT size)
    magnitude <- abs(fft_result[1:(fft_size/2 + 1)])
    spectrogram[, i] <- 20 * log10(magnitude + .Machine$double.eps)
  }

  # Frequency axis based on padded FFT size
  freq_axis <- seq(0, fs/2, length.out = fft_size/2 + 1)
  time_axis <- seq(0, length(signal)/fs, length.out = num_segments)

  # Apply dynamic range limitation
  max_val <- max(spectrogram)
  spectrogram[spectrogram < (max_val - dynamic.range)] <- max_val - dynamic.range

  # Calculate derived visualizations FROM THE SPECTROGRAM MATRIX USING SD
  if (osc.show) {
    if(osc.fun == "mean"){
      # Convert dB to linear scale before averaging
      linear_spec <- 10^(spectrogram/20)
      osc_data <- apply(linear_spec, 2, mean)
    } else {
      osc_data <- apply(spectrogram, 2, osc.fun)
    }
    osc_data <- osc_data / max(osc_data)  # Normalize
  }

  if (spec.show) {

    if (spec.linear) {

      spec_data <- apply(spectrogram, 1, spec.fun)
      # Convert from dB back to linear scale
      spec_data_linear <- 10^(spec_data/20)

      spec_data_linear <- (spec_data_linear - min(spec_data_linear)) / (max(spec_data_linear) - min(spec_data_linear))



    } else {

      # Power spectrum: SD per frequency bin (row-wise)
      spec_data <- apply(spectrogram, 1, spec.fun)
      spec_data <- (spec_data - min(spec_data)) / (max(spec_data) - min(spec_data))


    }



  }

  # Set up graphics device if output file is specified
  if (!is.null(output.file)) {
    if (use.ragg) {
      ragg::agg_png(output.file,
                    width = 1000,
                    height = 800, res = 150)
    } else {
      png(output.file, width = 1000,
          height = 800, res = 150)
    }
    on.exit(dev.off(), add = TRUE)
  }

  # Create color palette
  if (is.character(palette)) {
    # Try to create the palette and catch errors
    tryCatch({
      colors <- grDevices::hcl.colors(256, palette = palette, rev = palette_rev)
    }, error = function(e) {
      # Get available palettes
      available_palettes <- grDevices::hcl.pals()
      stop(sprintf("Invalid palette name: '%s'\nAvailable palettes are:\n%s\n\nDid you mean 'Grays' instead of 'greys'?",
                   palette,
                   paste(strwrap(paste(available_palettes, collapse = ", "), width = 70), collapse = "\n")),
           call. = FALSE)
    })
  } else if (is.function(palette)) {
    colors <- palette(256)
    if (palette_rev) {
      colors <- rev(colors)
    }
  } else {
    warning("Invalid palette specified. Using 'viridis' as default.")
    colors <- grDevices::hcl.colors(256, palette = "viridis", rev = palette_rev)
  }

  # Set up multi-panel layout
  if (osc.show && spec.show) {
    layout_matrix <- matrix(c(1, 3, 2, 4),
                            nrow = 2, ncol = 2,
                            byrow = TRUE)
    graphics::layout(layout_matrix,
                     widths = c(1 - spec.width, spec.width),
                     heights = c(1 - osc.height, osc.height))
  } else if (osc.show) {
    layout_matrix <- matrix(c(1, 2), nrow = 2, ncol = 1)
    graphics::layout(layout_matrix, heights = c(1 - osc.height, osc.height))
  } else if (spec.show) {
    layout_matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
    graphics::layout(layout_matrix, widths = c(1 - spec.width, spec.width))
  } else {
    graphics::layout(1)
  }

  # Main spectrogram plot
  par(mgp = c(1.4, 0.5, 0), mar = c(ifelse(osc.show, 0.5, 4), 4, 4,
                                    ifelse(spec.show, 0.5, 5)))
  # par(mgp = c(3, 0.3, 0))
  image(x = time_axis,
        y = freq_axis/1000,
        z = t(spectrogram),
        col = colors,
        xlim = tlim,
        ylim = flim,
        xlab = "",
        ylab = "Frequency (kHz)",
        main = plot.title,
        useRaster = TRUE,
        yaxt = "n",
        xaxt = ifelse(osc.show, "n", "s"))

  # Conditionally remove 0 tick
  # Get default tick positions
  y_ticks <- axTicks(2)

  # Determine if we should remove the 0 tick
  remove_zero <- is.null(flim) || flim[1] == 0

  if(remove_zero){
    y_ticks <- y_ticks[y_ticks != 0]
  }

  # Add the modified y-axis
  axis(2, at = y_ticks)

  # Add box around spectrogram
  box()

  # Pseudo-oscillogram (below spectrogram) - CLEAN STYLE
  if (osc.show) {

    osc.label <- switch(osc.fun,
                    "mean" = "Mean Amp.",
                    "sd" = "SD Amp.",
                    "max" = "Max Amp.",
                    "median" = "Median Amp.",
                    "Amplitude")

    par(mar = c(4, 4, 0.1, ifelse(spec.show, 0.5, 5)))
    plot(time_axis, osc_data, type = "n",
         xlab = "Time (s)", ylab = osc.label,
         ylim = c(0, 1), xlim = tlim,
         xaxs = "i", yaxs = "i", yaxt = "n",
         bty = "o")


    # Fill area with color (no background)
    polygon(c(time_axis, rev(time_axis)),
            c(osc_data, rep(0, length(osc_data))),
            col = osc.color, border = NA)

  }

  if (spec.show) {
    par(mar = c(ifelse(osc.show, 0.5, 4), 0.5, 4, 4))

    if (spec.linear) {
      plot(spec_data_linear, freq_axis/1000, type = "n",
           xlab = "Amp.", ylab = "",
           xlim = c(0, 1), ylim = flim,
           xaxs = "i", yaxs = "i",
           yaxt = "n", xaxt = "n", bty = "o")

      # Fill area
      polygon(c(spec_data_linear,
                rep(0, length(spec_data_linear))),
              c(freq_axis/1000, rev(freq_axis/1000)),
              col = spec.color, border = NA)
    } else {

      plot(spec_data, freq_axis/1000, type = "n",
           xlab = "Amp.", ylab = "",
           xlim = c(0, 1), ylim = flim,
           xaxs = "i", yaxs = "i",  # Remove duplicate yaxs
           yaxt = "n", xaxt = "n", bty = "o")

      # Fill area
      polygon(c(spec_data, rep(0, length(spec_data))),
              c(freq_axis/1000, rev(freq_axis/1000)),
              col = spec.color, border = NA)

    }

    # Add custom x-axis with 3 ticks at 0, 0.5, 1
    axis(1, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
  }

  # Reset layout to default
  graphics::layout(1)
  par(mar = c(5, 4, 4, 2) + 0.1)

  # Return spectrogram data invisibly
  invisible(list(
    spectrogram = spectrogram,
    time = time_axis,
    frequency = freq_axis,
    sample_rate = fs,
    osc_data = if (osc.show) osc_data else NULL,
    spec_data = if (spec.show) spec_data else NULL
  ))
}



