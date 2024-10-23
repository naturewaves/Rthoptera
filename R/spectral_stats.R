#' Calculate Spectral Statistics
#'
#' This function calculates various spectral statistics for a given audio signal, including peak frequency, bandwidth, spectral entropy, and spectral flatness. It generates an interactive plot with frequency and amplitude traces and includes options to visualize key statistics with lines on the plot.
#'
#' @param wave A `Wave` object representing the sound data.
#' @param specimen.id Character string for the specimen identifier.
#' @param total.range Logical, whether to calculate the full frequency range.
#' @param robust Logical, whether to use a robust frequency resolution.
#' @param ampMax Numeric, sets the amplitude scaling (1 for relative amplitude, 0 for dB).
#' @param dbth Numeric, decibel threshold for bandwidth calculation.
#' @param lines Logical, whether to add lines for min, max, and peak frequencies.
#' @param sound.type Character string for the type of sound analyzed.
#' @param temp Numeric, optional, temperature in degrees Celsius.
#' @param hpf Numeric, optional, high-pass filter cutoff frequency in kHz.
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
#' @importFrom seewave meanspec sh sfm
#' @importFrom plotly plot_ly add_lines layout add_trace add_markers
#' @importFrom tibble tibble
spectral_stats <- function(wave,
                           specimen.id = "Specimen ID",
                           total.range = FALSE,
                           robust = FALSE,
                           ampMax = 0,
                           dbth = -20,
                           lines = TRUE,
                           sound.type = "Call 1",
                           temp = NULL,
                           hpf = NULL) {

  ampMax <- as.numeric(ampMax)

  freq_per_bin <- if (robust) {
    244.1406
  } else {
    30.51758
  }

  sampling_rate <- wave@samp.rate
  wlen <- sampling_rate / freq_per_bin


  if(ampMax==0){
    spec1 <- meanspec(wave, wl = wlen, dB = NULL, plot = FALSE)
    spEnt <- sh(spec1)
    spFlat <- sfm(spec1)
    rm(spec1)
    spec <- meanspec(wave, wl = wlen, dB = "max0", plot = FALSE)
  }else{
    spec <- meanspec(wave, wl = wlen, dB = if (ampMax == 0) "max0" else NULL, plot = FALSE)
    spEnt <- sh(spec)
    spFlat <- sfm(spec)
  }

  spec_df <- as.data.frame(spec)
  names(spec_df) <- c("Frequency", "Amplitude")

  max_amp_index <- which.max(spec_df$Amplitude)
  peak_frequency <- spec_df$Frequency[max_amp_index]
  A_peak <- spec_df$Amplitude[max_amp_index]

  A_ref <- if (ampMax == 1) {
    A_peak * dbth
    # A_peak * 10^(dbth / 20)
  } else {
    A_peak - abs(dbth)
  }

  minfreq_index <- if (total.range) {
    which(spec_df$Amplitude[1:max_amp_index] >= A_ref)[1]
  } else {
    max(which(spec_df$Amplitude[1:max_amp_index] <= A_ref))
  }

  maxfreq_index <- if (total.range) {
    max_amp_index + which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref)[length(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref))]
  } else {
    max_amp_index + min(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] <= A_ref)) - 1
  }

  minfreq <- spec_df$Frequency[minfreq_index]
  maxfreq <- spec_df$Frequency[maxfreq_index]

  p <- plot_ly(
    spec_df,
    x = ~ Frequency,
    y = ~ Amplitude,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'black', width = 0.5),
    hovertemplate = paste("Frequency: %{x:.2f} kHz<br>Amplitude: %{y:.2f}<extra></extra>"),
    showlegend = FALSE
  )

  text_label <- paste(
    "<b> Summary Statistics</b>",
    "<br> Low Freq:", round(minfreq, 1), "kHz",
    "<br> Peak Freq:", round(peak_frequency, 1), "kHz",
    "<br> High Freq:", round(maxfreq, 1), "kHz",
    "<br> Bandwidth:", round(maxfreq - minfreq, 1), "kHz",
    "<br> Spectral Entropy:", round(spEnt,1),
    "<br> Spectral Flatness:", round(spFlat,1),
    "<br> Temp:", temp, "C",
    "<br> HPF:", hpf, "kHz"
  )

  p <- p %>%
    layout(
      yaxis = list(range = ifelse(ampMax == 0, c(-50, 0), c(0, 1))),
      margin = list(l = 50, r = 50, t = 100, b = 50),
      title = list(text = sprintf("<i>%s</i>", specimen.id), x = 0, y = 1.1, xref = "paper", yref = "paper", xanchor = 'left', yanchor = 'top'),
      xaxis = list(title = "Frequency (kHz)"),
      yaxis = list(title = ifelse(ampMax == 1, "Relative Amplitude", "Amplitude (dB)")),
      annotations = list(
        x = 1, y = .95, text = text_label, showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'right', yanchor = 'top',
        font = list(size = 12), bgcolor = 'rgba(255,255,255,1)', bordercolor = '#404040', align = 'left'
      )
    )

  if (lines) {
    p <- p %>%
      add_trace(
        x = c(minfreq, minfreq), y = c(min(spec_df$Amplitude), A_ref),
        type = 'scatter', mode = 'lines', line = list(color = "#1E90FF", width = 1, dash = 'solid'),
        name = "Min Frequency", hovertemplate = paste("MinFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
      ) %>%
      add_trace(
        x = c(peak_frequency, peak_frequency), y = c(min(spec_df$Amplitude), max(spec_df$Amplitude)),
        type = 'scatter', mode = 'lines', line = list(color = "#EE0000", width = 1, dash = 'solid'),
        name = "Peak Frequency", hovertemplate = paste("PeakFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
      ) %>%
      add_trace(
        x = c(maxfreq, maxfreq), y = c(min(spec_df$Amplitude), A_ref),
        type = 'scatter', mode = 'lines', line = list(color = "#FF7F00", width = 1, dash = 'solid'),
        name = "Max Frequency", hovertemplate = paste("MaxFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
      ) %>%
      add_trace(
        x = spec_df$Frequency, y = rep(A_ref, nrow(spec_df)),
        type = 'scatter', mode = 'lines', line = list(color = 'forestgreen', width = 1, dash = 'dash'),
        name = "dB Threshold", hovertemplate = paste("-20 dB Threshold <extra></extra>"), showlegend = TRUE
      )
  }

  p <- p %>% add_markers(
    x = peak_frequency, y = max(spec_df$Amplitude), type = 'scatter', mode = 'markers',
    marker = list(symbol = 'triangle-down', color = "#EE0000", size = 10), name = "Peak", showlegend = TRUE, hoverinfo = 'none', inherit = FALSE
  )

  scaling <- ifelse(ampMax == 1, "max1", "max0")
  q_factor <- peak_frequency / (maxfreq - minfreq)

  df <- tibble(
    specimen.id = specimen.id,
    sound.type = sound.type,
    low.f = round(minfreq, 1),
    high.f = round(maxfreq, 1),
    bandw = round(maxfreq - minfreq, 1),
    peak.f = round(peak_frequency, 1),
    q = round(q_factor, 1),
    spec.ent = round(spEnt, 1),
    spec.flat = round(spFlat,1),
    temp = temp,
    par.hpf = hpf,
    par.dbth = dbth,
    par.samprate = sampling_rate / 1000,
    par.wlen = round(wlen),
    par.freq.res = round(freq_per_bin, 1),
    par.robust = robust,
    par.scale = scaling
  )

  list(data = df, plot = p)
}
