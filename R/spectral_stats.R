#' Calculate Spectral Statistics
#'
#' This function calculates various spectral statistics for a complete Wave
#' object, including peak frequency, bandwidth, spectral entropy, spectral
#' flatness, spectral excursion, and spectral energy. It generates an
#' interactive plot with frequency and amplitude traces and includes options
#' to visualize key statistics with lines on the plot.
#'
#' @param wave A `Wave` object representing the sound data.
#' @param specimen_id Character string for the specimen identifier.
#' @param total_range Logical, whether to calculate the full frequency range.
#' @param robust Logical, whether to use a robust frequency resolution (244 Hz).
#' @param db Logical. If TRUE, a decibel scale is used. If FALSE (default),
#' a linear scale is used. Both are relative to the peak amplitude in the Wave.
#' @param cutoff Numeric, threshold for bandwidth calculation. If db = FALSE,
#' set between 0 and 1. If db = TRUE, set to a negative dB value.
#' @param lines Logical, whether to add lines for min, max, and peak
#' frequencies.
#' @param sound_type Character string for the type of sound analyzed.
#' @param temp Numeric, optional, temperature in degrees Celsius.
#' @param hpf Numeric, optional, high-pass filter cutoff frequency in kHz.
#' Defaults to 0.
#'
#' @return A list containing:
#' - `data`: A tibble with calculated statistics.
#' - `plot`: An interactive `plotly` plot with frequency and amplitude traces.
#' @export
#'
#' @examples
#' \dontrun{
#' wave <- readWave("example.wav")
#' result <- spectral_stats(wave)
#' result$plot
#' }
#'
#' @importFrom dplyr mutate filter tibble
#' @importFrom utils head tail
#' @importFrom seewave meanspec sh sfm
#' @importFrom plotly plot_ly add_lines layout add_trace add_markers
#' @importFrom tibble tibble
#' @importFrom htmlwidgets onRender
spectral_stats <- function(wave,
                           specimen_id = "",
                           total_range = FALSE,
                           robust = TRUE,
                           db = FALSE,
                           cutoff = 0.5,
                           lines = TRUE,
                           sound_type = "",
                           temp = NULL,
                           hpf = 0) {

  if (db == FALSE && cutoff <= 0) {
    stop("When db = FALSE, cutoff should be between 0 and 1. The equivalent to
         -20dB is 0.1 (10% of the peak amplitude) \n")
  }

  if (db == TRUE && cutoff >= 0) {
    stop("When db = TRUE, cutoff should be a negative dB value. \n")
  }


  freq_per_bin <- if (robust) {
    244.1406 # equivalent to a window length of 1024 & samp. rate of 250 kHz
  } else {
    30.51758 # equivalent to window length = 8192 & samp. rate of 250 kHz
  }

  sampling_rate <- wave@samp.rate
  wlen <- sampling_rate / freq_per_bin


  if (db) {
    # Compute Spectral Indices with linear mean power spectrum
    spec1 <- meanspec(wave, wl = wlen, dB = NULL, plot = FALSE)
    sp.ent <- sh(spec1)
    sp.flat <- sfm(spec1)
    rm(spec1)
    # Store the dB mean power spectrum
    spec <- meanspec(wave, wl = wlen, dB = "max0", plot = FALSE)
  } else {
    spec <- meanspec(wave, wl = wlen, dB = NULL, plot = FALSE)
    sp.ent <- sh(spec)
    sp.flat <- sfm(spec)
  }

  spec_df <- as.data.frame(spec)
  names(spec_df) <- c("Frequency", "Amplitude")

  max_amp_index <- which.max(spec_df$Amplitude)
  peak_frequency <- spec_df$Frequency[max_amp_index]
  A_peak <- spec_df$Amplitude[max_amp_index]

  A_ref <- if (db) {
    A_peak - abs(cutoff)
  } else {
    A_peak * cutoff
  }

  minfreq_index <- if (total_range) {
    which(spec_df$Amplitude[1:max_amp_index] >= A_ref)[1]
  } else {
    max(which(spec_df$Amplitude[1:max_amp_index] <= A_ref))
  }

  # Get amplitudes above the reference threshold
  amp_values <- spec_df$Amplitude[max_amp_index:nrow(spec_df)]
  high_amplitude <- amp_values >= A_ref

  # Handle the two cases separately
  maxfreq_index <- if (total_range) {
    max_amp_index + which(high_amplitude)[length(which(high_amplitude))]
  } else {
    max_amp_index + min(which(!high_amplitude)) - 1
  }


  minfreq <- spec_df$Frequency[minfreq_index]
  maxfreq <- spec_df$Frequency[maxfreq_index]


  # Calculate spec.exc and spec.en based on conditional parameters
  if (robust == TRUE && cutoff == -20) {
    # Use existing meanspec values if robust = TRUE and cutoff = -20
    freq_range <- spec_df[minfreq_index:maxfreq_index, ]
  } else {
    # Calculate a separate meanspec for spec.exc and auc with fixed robust parameters
    freq_per_bin_exc <- 244.1406  # Fixed 'robust' setting
    wlen_exc <- wave@samp.rate / freq_per_bin_exc
    spec_exc <- meanspec(wave, wl = wlen_exc, dB = "max0", plot = FALSE)
    spec_df_exc <- as.data.frame(spec_exc)
    names(spec_df_exc) <- c("Frequency", "Amplitude")

    # Find indices for low and high frequencies in separate meanspec
    max_amp_index_exc <- which.max(spec_df_exc$Amplitude)
    A_peak_exc <- spec_df_exc$Amplitude[max_amp_index_exc]
    A_ref_exc <- A_peak_exc - abs(cutoff)
    minfreq_index_exc <- max(which(spec_df_exc$Amplitude[1:max_amp_index_exc] <= A_ref_exc))
    high_amplitude_exc <- spec_df_exc$Amplitude[max_amp_index_exc:nrow(spec_df_exc)] >= A_ref_exc
    maxfreq_index_exc <- max_amp_index_exc + min(which(!high_amplitude_exc)) - 1

    freq_range <- spec_df_exc[minfreq_index_exc:maxfreq_index_exc, ]
  }

  # Spectral Excursion (contour length)
  spec_ex <- sum(sqrt(diff(freq_range$Frequency)^2 + diff(freq_range$Amplitude)^2))

  # Spectral energy (area under the curve)
  spec_en <- abs(sum(diff(freq_range$Frequency) * (head(freq_range$Amplitude, -1) + tail(freq_range$Amplitude, -1)) / 2))


  scaling <- ifelse(db, "db", "linear")
  q_factor <- peak_frequency / (maxfreq - minfreq)

  df <- tibble(
    specimen.id = specimen_id,
    sound.type = sound_type,
    low.f = round(minfreq, 1),
    high.f = round(maxfreq, 1),
    bandw = round(maxfreq - minfreq, 1),
    peak.f = round(peak_frequency, 1),
    sp.exc = round(spec_ex, 1),
    sp.ene = round(spec_en, 1),
    sp.ent = round(sp.ent, 2),
    sp.flat = round(sp.flat, 2),
    q.factor = round(q_factor, 1),
    temp = temp,
    par.hpf = hpf,
    par.cutoff = cutoff,
    par.s.rate = sampling_rate / 1000,
    par.wlen = round(wlen),
    par.freq.res = round(freq_per_bin, 1),
    par.robust = robust,
    par.scale = scaling
  )


  # PLOTTING
  p <- plot_ly(spec_df) |>
    add_lines(
      x = ~Frequency, y = ~Amplitude, name = "Summary Statistics",
      hoverinfo = "none", line = list(
        color = "rgba(20, 20, 20, 0)",
        width = 2
      ), legendgroup = "Summary Stats"
    ) |>
    add_lines(
    x = ~Frequency,
    y = ~Amplitude,
    type = "scatter",
    mode = "lines",
    line = list(color = 'rgba(0,0,0,0.2)', width = 1),
    fill = "tozeroy",
    fillcolor = 'rgba(0,0,0,0.6)',
    hovertemplate = paste(
      "Frequency: %{x:.2f} kHz<br>Amplitude: %{y:.2f}<extra></extra>"
    ),
    showlegend = FALSE
  )

  text_label <- paste(
    "<b> Summary Statistics</b>",
    "<br> Peak Freq:", df$peak.f, "kHz",
    "<br> Low Freq:", df$low.f, "kHz",
    "<br> High Freq:", df$high.f, "kHz",
    "<br> Bandwidth:", df$bandw, "kHz",
    "<br> Sp. Excursion:", df$sp.exc,
    "<br> Sp. Energy:", df$sp.ene,
    "<br> Sp. Entropy:", df$sp.ent,
    "<br> Sp. Flatness:", df$sp.flat,
    "<br> Temp:", temp, "C",
    "<br> HPF:", hpf, "kHz"
  )

  p <- p |>
    layout(
      # yaxis = list(range = ifelse(db, c(-10, 0), c(0.5, 1))),  # Set the range for y-axis
      yaxis = list(range = ifelse(db, list(-10, 0), list(0.5, 1))),
      margin = list(l = 50, r = 50, t = 100, b = 50),
      title = list(
        text = sprintf("<i>%s</i>", specimen_id), x = 0, y = 1.1,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top"
      ),
      xaxis = list(title = "Frequency (kHz)"),
      yaxis = list(
        title = ifelse(db, "Amplitude (dB)", "Linear Amplitude")
      ),
      annotations = list(
        x = 1, y = .95, text = text_label, showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        font = list(size = 12), bgcolor = "rgba(255,255,255,1)",
        bordercolor = "#404040", align = "left"
      )
    )

  if (lines) {
    p <- p |>
      add_trace(
        x = c(minfreq, minfreq), y = c(min(spec_df$Amplitude), A_ref),
        type = "scatter", mode = "lines",
        line = list(color = "#1E90FF", width = 2, dash = "solid"),
        name = "Min Frequency",
        hovertemplate = paste("MinFreq: %{x} kHz <extra></extra>"),
        showlegend = TRUE
      ) |>
      add_trace(
        x = c(peak_frequency, peak_frequency), y = c(
          min(spec_df$Amplitude),
          max(spec_df$Amplitude)
        ),
        type = "scatter", mode = "lines",
        line = list(color = "#EE0000", width = 1, dash = "solid"),
        name = "Peak Frequency",
        hovertemplate = paste("PeakFreq: %{x} kHz <extra></extra>"),
        showlegend = TRUE
      ) |>
      add_trace(
        x = c(maxfreq, maxfreq), y = c(min(spec_df$Amplitude), A_ref),
        type = "scatter", mode = "lines",
        line = list(color = "#FF7F00", width = 2, dash = "solid"),
        name = "Max Frequency",
        hovertemplate = paste("MaxFreq: %{x} kHz <extra></extra>"),
        showlegend = TRUE
      ) |>
      add_trace(
        x = spec_df$Frequency, y = rep(A_ref, nrow(spec_df)),
        type = "scatter", mode = "lines",
        line = list(
          color = "forestgreen",
          width = 1.5, dash = "dash"
        ),
        name = "dB Threshold",
        hovertemplate = paste("dB Threshold <extra></extra>"),
        showlegend = TRUE
      )
  }

  p <- p |> add_markers(
    x = peak_frequency, y = max(spec_df$Amplitude), type = "scatter",
    mode = "markers",
    marker = list(symbol = "triangle-down", color = "#EE0000", size = 10),
    name = "Peak", showlegend = TRUE, hoverinfo = "none", inherit = FALSE
  )

  p <- htmlwidgets::onRender(p, "
   function(el, x) {
     el.on('plotly_restyle', function(d) {
      var traceVisible = x.data[0].visible;
      var annotations = x.layout.annotations;

      if (traceVisible === true || traceVisible === undefined) {
        annotations[0].visible = true;
      } else {
        annotations[0].visible = false;
      }

      // Apply the updated annotation visibility
      Plotly.relayout(el, {annotations: annotations});
      });
   }
   ")



  list(data = df, plot = p)
}
