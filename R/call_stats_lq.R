#' Temporal and spectral statistics of detected broadband calls.
#'
#' This function calculates temporal and spectral statistics, identifying motifs
#' and trains in the waveform, detecting peaks, and extracting spectral features
#' such as peak frequency, low frequency, high frequency, and bandwidth.
#'
#' @param wave A `Wave` object.
#' @param specimen_id A character string representing the identifier for the
#' specimen (or recording) being analyzed.
#' @param ssmooth Numeric. Smoothing window size (in samples) for the amplitude
#' envelope.
#' @param peakfinder_ws Numeric. Window size (in samples) for the peak detection
#' algorithm.
#' @param peakfinder_threshold Numeric. Amplitude threshold for peak detection,
#' as a proportion of the maximum amplitude.
#' @param max_peak_gap Numeric. Maximum allowed gap (in seconds) between peaks
#' to consider them part of the same train.
#' @param max_train_gap Numeric. Maximum allowed gap (in seconds) between trains
#' to consider them part of the same motif.
#' @param motif_seq Logical. If TRUE, add an first-order aggregation of motifs.
#' Default = FALSE.
#' @param max_motif_gap Numeric. Maximum allowed gap (in seconds) between motifs
#' to consider them part of the same group.
#' @param detection_threshold Numeric. Minimum amplitude for a peak to be
#' considered valid.
#' @param norm_env Logical. Whether to normalize the amplitude envelope between
#' 0 and 1. Defaults to `TRUE`.
#' @param db_threshold Numeric. Decibel threshold below the peak amplitude for
#' calculating low and high frequencies. Defaults to 20 dB.
#' @return A list containing the following components:
#'   \item{plot}{An interactive `plotly` object showing the waveform envelope,
#'   detected peaks, trains, and motifs.}
#'   \item{summary_data}{A tibble with summary statistics for the analyzed
#'   waveform.}
#'   \item{motif_seq_data}{A tibble summarizing motif sequences. Only produced
#'   when motif_seq = TRUE.}
#'   \item{motif_data}{A tibble summarizing motifs, including motif duration,
#'   train counts, and spectral properties.}
#'   \item{train_data}{A tibble detailing detected trains, including start and
#'   end times, durations, peak frequencies, and bandwidths.}
#'   \item{peak_data}{A tibble containing detected peaks, including their times,
#'   periods, and amplitudes.}
#'   \item{params}{A tibble summarizing input parameters for the function.}
#' @examples
#' \dontrun{
#' data(coryphoda)
#' # Perform analysis on the coryphoda sample
#' results <- call_stats_lq(
#'   wave = coryphoda,
#'   specimen_id = "Coryphoda_Sample",
#'   ssmooth = 100,
#'   peakfinder_ws = 50,
#'   peakfinder_threshold = 0.01,
#'   max_train_gap = 0.5,
#'   max_peak_gap = 0.05,
#'   detection_threshold = 0.2,
#'   norm_env = TRUE,
#'   db_threshold = 20
#' )
#' results$plot
#' }
#' @export
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr reframe rowwise mutate select ungroup tibble
call_stats_lq <- function(wave,
                          specimen_id = "",
                          ssmooth = 100,
                          peakfinder_ws = 50,
                          peakfinder_threshold = 0.005,
                          max_peak_gap = 0.01,
                          max_train_gap = 0.5,
                          motif_seq = TRUE,
                          max_motif_gap = 0.8,
                          detection_threshold = 0.1,
                          norm_env = TRUE,
                          db_threshold = 20) {

  # Store input parameters in a tibble
  params <- tibble(
    specimen_id = specimen_id,
    ssmooth = ssmooth,
    peakfinder_ws = peakfinder_ws,
    peakfinder_threshold = peakfinder_threshold,
    max_train_gap = max_train_gap,
    max_peak_gap = max_peak_gap,
    norm_env = norm_env
  )

  window_size <- peakfinder_ws

  waveDuration <- seewave::duration(wave)


  if (norm_env) {
    envelope_vector <- seewave::env(wave, ssmooth = ssmooth,
                                    norm = TRUE, plot = FALSE)
  } else {
    envelope_vector <- seewave::env(wave, ssmooth = ssmooth, plot = FALSE)
  }

  max_amplitude <- max(envelope_vector)
  amp_threshold <- max_amplitude * peakfinder_threshold
  peaks <- c()

  for (i in (window_size + 1):(length(envelope_vector) - window_size)) {
    window <- envelope_vector[(i - window_size):(i + window_size)]
    center_value <- envelope_vector[i]
    if (center_value == max(window) && (center_value - min(window)) > amp_threshold) {
      peaks <- c(peaks, i)
    }
  }

  # Create the time vector
  sample_rate <- wave@samp.rate
  time_vector <- seq(0, (length(wave@left) - 1) / sample_rate, length.out = length(envelope_vector)) # In seconds

  peaks <- peaks[peaks <= length(time_vector)]


  # Filter out peaks below the detection threshold
  peak_amplitudes <- envelope_vector[peaks] # Get amplitudes of the detected peaks
  valid_peaks <- which(peak_amplitudes >= detection_threshold) # Only keep peaks above threshold
  peaks <- peaks[valid_peaks] # Update the peaks list
  peak_amplitudes <- peak_amplitudes[valid_peaks] # Update amplitudes list

  peak_times <- time_vector[peaks]

  peak_periods <- diff(peak_times)
  peak_periods[peak_periods > max_peak_gap] <- NA

  peak_data <- tibble(
    specimen.id = rep(specimen_id, length(peaks)),
    motif.id = integer(length(peaks)),
    train.id = integer(length(peaks)),
    peak.id = integer(length(peaks)),
    peak.time = peak_times,
    peak.period = c(peak_periods, NA),
    peak.amp = round(peak_amplitudes, 4)
  )

  trains <- list()
  motifs <- list()
  motif_id <- 1
  train_id <- 1
  train_start <- peaks[1]
  current_motif <- list(c(train_start, NULL))

  peak_data <- peak_data |>
    mutate(
      motif.id = ifelse(row_number() == 1, motif_id, motif.id),
      train.id = ifelse(row_number() == 1, train_id, train.id),
      peak.id = ifelse(row_number() == 1, 1, peak.id)
    )

  peak_data <- peak_data |>
    mutate(peak.period = round(peak.period, 4))

  peak_counter <- 1

  for (i in 2:length(peaks)) {
    if (peak_times[i] - peak_times[i - 1] > max_peak_gap) {
      train_end <- peaks[i - 1]
      trains <- append(trains, list(c(train_start, train_end)))
      train_start <- peaks[i]
      current_motif[[length(current_motif)]][2] <- train_end
      current_motif <- append(current_motif, list(c(train_start, NULL)))
      peak_counter <- 0
      if (peak_times[i] - peak_times[i - 1] > max_train_gap) {
        motifs <- append(motifs, list(current_motif))
        current_motif <- list(c(train_start, NULL))
        motif_id <- motif_id + 1
        train_id <- 1
      } else {
        train_id <- train_id + 1
      }
    }
    peak_counter <- peak_counter + 1
    peak_data <- peak_data |>
      mutate(
        motif.id = ifelse(peak.time == peak_times[i], motif_id, motif.id),
        train.id = ifelse(peak.time == peak_times[i], train_id, train.id),
        peak.id = ifelse(peak.time == peak_times[i], peak_counter, peak.id)
      )
  }

  # Round peak.time to 4 decimals
  peak_data <- peak_data |>
    mutate(peak.time = round(peak.time, 4))

  train_end <- peaks[length(peaks)]
  trains <- append(trains, list(c(train_start, train_end)))
  current_motif[[length(current_motif)]][2] <- train_end
  motifs <- append(motifs, list(current_motif))

  # Create tibble for peak train measurements
  train_data <- peak_data |>
    group_by(specimen.id, motif.id, train.id) |>
    summarize(
      spcimen.id = unique(specimen.id),
      train.start = min(peak.time),
      train.end = max(peak.time),
      train.dur = round((train.end - train.start), 3),
      n.peaks = n(),
      mean.amp = round(mean(peak.amp), 3)
    ) |>
    mutate(peak.rate = round(((n.peaks - 1) / train.dur), 1)) |>
    ungroup() |>
    mutate(
      train.period = ifelse(is.na(lead(motif.id)) | lead(motif.id) != motif.id, NA, lead(train.start) - train.start),
      train.gap = round(lead(train.start) - train.end, 3)
    ) |>
    relocate(train.period, .after = train.dur) |>
    relocate(train.gap, .after = train.period) |>
    mutate(train.period = round(train.period, 3))


  # Calculate temporal excursion (variability in timing of peaks, per train)
  # Add peak period column in milliseconds
  peak_data$peak.period.ms <- round((peak_data$peak.period * 1000), 4)

  tem_exc_data <- peak_data |>
    group_by(train.id) |>
    summarize(tem.exc = round(sum(abs(diff(peak.period.ms)), na.rm = TRUE), 3))

  train_data <- train_data |>
    left_join(tem_exc_data, by = "train.id")


  # Calculate tynamic excutsion (variability in energy among peaks, per train)
  # Create the peak_dyn_data data frame
  peak_dyn_data <- peak_data |>
    group_by(train.id) |>
    mutate(peak.diff = abs(peak.amp - lag(peak.amp))) |> # Calculate differences between consecutive peak.amp values
    filter(!is.na(peak.diff)) # Remove rows where peak.diff is NA (first peak in each train)

  # Sum the absolute differences for each train.id
  dyn_exc_data <- peak_dyn_data |>
    group_by(train.id) |>
    summarize(dyn.exc = round(sum(peak.diff, na.rm = TRUE), 3))


  # Add dyn.exc to train_data by matching train.id
  train_data <- train_data |>
    left_join(dyn_exc_data, by = "train.id")

  train_data <- train_data |>
    relocate(c(tem.exc, dyn.exc), .after = train.id)

  # Add Spectral Statistics for each train
  train_data <- train_data |>
    rowwise() |>
    mutate(
      spectral_stats = list(
        tryCatch(
          {

            spec1 <- meanspec(wave, wl = 128,
                              from = train.start,
                              to = train.end,
                              dB = NULL, plot = FALSE)
            sp.ent <- sh(spec1)
            sp.flat <- sfm(spec1)
            rm(spec1)

            # Calculate meanspec for the train
            spec <- meanspec(wave,
                             from = train.start,
                             to = train.end,
                             wl = 128,
                             dB = "max0",
                             plot = FALSE)

            # Convert the spectrum to a data frame
            spec_df <- as.data.frame(spec)
            names(spec_df) <- c("Frequency", "Amplitude")

            # Identify the peak frequency and peak amplitude
            peak_index <- which.max(spec_df$Amplitude)
            peak.freq <- round(spec_df$Frequency[peak_index], 1)
            peak_amp <- spec_df$Amplitude[peak_index]

            # Determine the threshold (dB below the peak amplitude)
            threshold <- peak_amp - db_threshold

            # Find low frequency (first frequency below threshold moving to lower frequencies)
            low_index <- max(which(spec_df$Amplitude[1:peak_index] < threshold))
            low.freq <- round(spec_df$Frequency[low_index], 1)

            # Find high frequency (first frequency below threshold moving to higher frequencies)
            high_index <- peak_index + min(which(spec_df$Amplitude[peak_index:nrow(spec_df)] < threshold)) - 1
            high.freq <- round(spec_df$Frequency[high_index], 1)

            # Calculate bandwidth
            bandw <- round(high.freq - low.freq, 1)

            freq_range <- spec_df[low_index:high_index, ]


            # Spectral Excursion (contour length)
            sp.exc <- sum(sqrt(diff(freq_range$Frequency)^2 + diff(freq_range$Amplitude)^2))

            # Spectral energy (area under the curve)
            sp.ene <- abs(sum(diff(freq_range$Frequency) * (head(freq_range$Amplitude, -1) + tail(freq_range$Amplitude, -1)) / 2))




            tibble(peak.freq, low.freq, high.freq, bandw, sp.exc, sp.ene, sp.ent, sp.flat)



          },
          error = function(e) {

          }
        )
      )
    ) %>%
    ungroup() %>%
    # Expand the spectral stats into individual columns
    unnest_wider(spectral_stats)

  # Create tibble for motif measurements
  motif_data <- train_data |>
    group_by(motif.id) |>
    reframe(
      motif.start = min(train.start),
      motif.end = max(train.end),
      motif.dur = round(motif.end - motif.start, 4),
      motif.period = round((lead(motif.start) - motif.start), 4),
      n.trains = n(),
      duty.cycle = round((sum(train.dur) / motif.dur) * 100, 2),
      tem.exc.mean = round(mean(tem.exc), 3),
      tem.exc.var = round(var(tem.exc), 3),
      tem.exc.sd = round(sd(tem.exc), 3),
      dyn.exc.mean = round(mean(dyn.exc), 3),
      dyn.exc.var = round(var(dyn.exc), 3),
      dyn.exc.sd = round(sd(dyn.exc), 3),
      peak.freq.mean = round(mean(peak.freq), 3),
      peak.freq.var = round(var(peak.freq), 3),
      peak.freq.sd = round(sd(peak.freq), 3),
      low.freq.mean = round(mean(low.freq),3),
      low.freq.var = round(var(low.freq), 3),
      low.freq.sd = round(sd(low.freq), 3),
      high.freq.mean = round(mean(high.freq),3),
      high.freq.var = round(var(high.freq), 3),
      high.freq.sd = round(sd(high.freq), 3),
      bandw.mean = round(mean(bandw),3),
      bandw.var = round(var(bandw), 3),
      bandw.sd = round(sd(bandw), 3),
      sp.exc.mean = round(mean(sp.exc, na.rm = TRUE), 3),
      sp.exc.var = round(var(sp.exc, na.rm = TRUE), 3),
      sp.exc.sd = round(sd(sp.exc, na.rm = TRUE), 3),
      sp.ene.mean = round(mean(sp.ene, na.rm = TRUE), 3),
      sp.ene.var = round(var(sp.ene, na.rm = TRUE), 3),
      sp.ene.sd = round(sd(sp.ene, na.rm = TRUE), 3),
      sp.ent.mean = round(mean(sp.ent, na.rm = TRUE), 3),
      sp.ent.var = round(var(sp.ent, na.rm = TRUE), 3),
      sp.ent.sd = round(sd(sp.ent, na.rm = TRUE), 3),
      sp.flat.mean = round(mean(sp.flat, na.rm = TRUE), 3),
      sp.flat.var = round(var(sp.flat, na.rm = TRUE), 3),
      sp.flat.sd = round(sd(sp.flat, na.rm = TRUE), 3)
    ) |>
    mutate(
      train.rate = round((n.trains - 1) / motif.dur)
    ) |>
    relocate(train.rate, .after = n.trains) |>
    relocate(c(tem.exc.mean:dyn.exc.sd), .after = motif.id) |>
    ungroup()


  motif_data <- motif_data |>
    mutate(
      proportions = map(motif.id, function(eid) {
        train_durations <- train_data |>
          filter(motif.id == eid) |>
          pull(train.dur)
        gap_durations <- train_data |>
          filter(motif.id == eid) |>
          pull(train.gap)
        motif_start <- motif_data |>
          filter(motif.id == eid) |>
          pull(motif.start)
        motif_end <- motif_data |>
          filter(motif.id == eid) |>
          pull(motif.end)
        motif_duration <- motif_data$motif.dur[eid]
        proportions <- numeric(0)

        for (i in seq_along(train_durations)) {
          # Add train duration as a proportion of the motif duration
          proportions <- c(proportions, train_durations[i] / motif_duration)

          # Check if the gap is not NA and falls within the motif start and end
          if (!is.na(gap_durations[i])) {
            gap_start <- train_data |>
              filter(motif.id == eid) |>
              pull(train.end) |>
              nth(i)
            gap_end <- train_data |>
              filter(motif.id == eid) |>
              pull(train.start) |>
              nth(i + 1)

            # Check if gap_start, gap_end, motif_start, and motif_end are not NA
            if (!is.na(gap_start) && !is.na(gap_end) && !is.na(motif_start) && !is.na(motif_end)) {
              if (gap_start >= motif_start && gap_end <= motif_end) {
                proportions <- c(proportions, gap_durations[i] / motif_duration)
              }
            }
          }
        }

        round(proportions, 2)
      })
    ) |>
    rowwise() |>
    mutate(
      specimen.id = base::unique(train_data$specimen.id),
      props.sd = round(sd(unlist(proportions)), 3),
      props.ent = round(-sum(unlist(proportions)[unlist(proportions) > 0] * log(unlist(proportions)[unlist(proportions) > 0])), 3),
      props.mean = round(mean(unlist(proportions)), 3),
      props.cv = round((props.sd / props.mean), 3),
      props.diff.sd = round(sd(diff(unlist(proportions))), 3),
      pci = round((props.ent * props.cv + sqrt(n.trains)) / (sqrt(motif.dur) + 1), 3)
    ) |>
    ungroup() |>
    select(specimen.id, motif.id, pci, everything(), -proportions, proportions)

  # Aggregate motifs
  if (motif_seq) {
  motif_data <- motif_data |>
    mutate(
      motif.seq = 1 + cumsum(ifelse(
        c(0, diff(motif.start)) > max_motif_gap,
        1, 0
      ))
    )

  motif_data <- motif_data |>
    mutate(
      motif.period = ifelse(is.na(lead(motif.seq)) | lead(motif.seq) != motif.seq, NA, lead(motif.start) - motif.start),
      motif.gap = round(lead(motif.start) - motif.end, 3)
    ) |>
    relocate(motif.period, .after = motif.dur) |>
    relocate(motif.gap, .after = motif.period) |>
    relocate(motif.seq, .after = motif.id)


    motif_data <- motif_data %>%
      group_by(motif.seq) %>%
      mutate(motif.id = row_number()) %>%
      ungroup()

    # Create motif.seq data for plotting
    motif_seq_data <- motif_data |>
      group_by(motif.seq) |>
      reframe(
        group.start = min(motif.start),
        group.end = max(motif.end)
      )

    motif_seq_data <- motif_seq_data |>
      mutate(
        group.dur = group.end - group.start,
        group.period = lead(group.start) - group.start,
        group.gap = round(lead(group.start) - group.end, 3)
      )
  }


  summary_data <- tibble(
    specimen.id = base::unique(train_data$specimen.id),
    n.motifs = nrow(motif_data),
    pci.mean = round(mean(motif_data$pci, na.rm = TRUE), 3),
    pci.sd = round(sd(motif_data$pci, na.rm = TRUE), 3),
    duty.cycle.mean = round(mean(motif_data$duty.cycle, na.rm = TRUE), 3),
    duty.cycle.sd = round(sd(motif_data$duty.cycle, na.rm = TRUE), 3),
    motif.dur.mean = round(mean(motif_data$motif.dur, na.rm = TRUE), 3),
    motif.dur.sd = round(sd(motif_data$motif.dur, na.rm = TRUE), 3),
    n.trains.mean = round(mean(motif_data$n.trains, na.rm = TRUE), 3),
    n.trains.sd = round(sd(motif_data$n.trains, na.rm = TRUE), 3),
    train.rate.mean = round(mean(motif_data$train.rate, na.rm = TRUE), 3),
    train.rate.sd = round(sd(motif_data$train.rate, na.rm = TRUE), 3),
    train.dur.mean = round(mean(train_data$train.dur, na.rm = TRUE), 3),
    train.dur.sd = round(sd(train_data$train.dur, na.rm = TRUE), 3),
    gap.dur.mean = round(mean(train_data$train.gap[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3),
    gap.dur.sd = round(sd(train_data$train.gap[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3),
    temp.exc.mean = round(mean(train_data$tem.exc, na.rm = TRUE), 3),
    temp.exc.sd = round(sd(train_data$tem.exc, na.rm = TRUE), 3),
    dyn.exc.mean = round(mean(train_data$dyn.exc, na.rm = TRUE), 3),
    dyn.exc.sd = round(sd(train_data$dyn.exc, na.rm = TRUE), 3),
    entropy.mean = round(mean(motif_data$props.ent, na.rm = TRUE), 3),
    entropy.sd = round(sd(motif_data$props.ent, na.rm = TRUE), 3),
    peak.freq.mean = round(mean(train_data$peak.freq, na.rm = TRUE), 3),
    peak.freq.sd = round(sd(train_data$peak.freq, na.rm = TRUE), 3),
    low.freq.mean = round(mean(train_data$low.freq, na.rm = TRUE), 3),
    low.freq.sd = round(sd(train_data$low.freq, na.rm = TRUE), 3),
    high.freq.mean = round(mean(train_data$high.freq, na.rm = TRUE), 3),
    high.freq.sd = round(sd(train_data$high.freq, na.rm = TRUE), 3),
    bandw.mean = round(mean(train_data$bandw, na.rm = TRUE), 3),
    bandw.sd = round(sd(train_data$bandw, na.rm = TRUE), 3),
    sp.exc.mean = round(mean(train_data$sp.exc, na.rm = TRUE), 3),
    sp.exc.sd = round(sd(train_data$sp.exc, na.rm = TRUE), 3),
    sp.ene.mean = round(mean(train_data$sp.ene, na.rm = TRUE), 3),
    sp.ene.sd = round(sd(train_data$sp.ene, na.rm = TRUE), 3),
    sp.ent.mean = round(mean(train_data$sp.ent, na.rm = TRUE), 3),
    sp.ent.sd = round(sd(train_data$sp.ent, na.rm = TRUE), 3),
    sp.flat.mean = round(mean(train_data$sp.flat, na.rm = TRUE), 3),
    sp.flat.sd = round(sd(train_data$sp.flat, na.rm = TRUE), 3)
  )

  if (motif_seq) {

    summary_data <- summary_data |>
      mutate(
    motif.seq.dur.mean = round(mean(motif_seq_data$group.dur, na.rm = TRUE), 3),
    motif.seq.dur.sd = round(sd(motif_seq_data$group.dur, na.rm = TRUE), 3)
        )

  }

  # Prepare annotations for the plot
  annotations <- list(
    list(
      x = 0.01,
      y = 0.99,
      xref = "paper",
      yref = "paper",
      text = paste(
        "<b> Summary Statistics</b>",
        "<br> N. motifs:", length(motifs),
        "<br> PCI: ", mean(motif_data$pci, 3),
        "<br> Duty Cycle: ", round(mean(motif_data$duty.cycle), 1), "%",
        "<br> Peak Freq: ", summary_data$peak.freq.mean, "kHz",
        "<br> Motif Dur.: ", round(mean(motif_data$motif.dur), 3), "s",
        "<br> Trains/Motif: ", round(mean(motif_data$n.trains), 1),
        "<br> Train Rate: ", round(mean(motif_data$train.rate)), "Hz",
        "<br> Train Dur.: ", round(mean(train_data$train.dur, na.rm = TRUE),3), "ms",
        "<br> Train Gap Dur.: ", round(summary_data$gap.dur.mean), "ms",
        "<br> Peaks/Train: ", round(mean(train_data$n.peaks, na.rm = TRUE)),
        "<br> Peak Rate: ", round(mean(train_data$peak.rate, na.rm = TRUE)), "Hz"
      ),
      showarrow = FALSE,
      font = list(size = 12),
      align = "left",
      bgcolor = "rgba(255, 255, 255, 0.5)",
      bordercolor = "rgba(0, 0, 0, 0.5)",
      borderwidth = 1,
      opacity = 1,
      visible = TRUE
    )
  )

  # Start the interactive plot
  p <- plot_ly() |>
    add_lines(
      x = ~time_vector, y = ~envelope_vector, name = "Summary Statistics",
      hoverinfo = "none", line = list(
        color = "rgba(20, 20, 20, 0)",
        width = 2
      ), legendgroup = "Summary Stats"
    ) |>
    add_lines(
      x = ~time_vector, y = ~envelope_vector, name = "Envelope",
      hoverinfo = "none", line = list(
        color = "rgb(20, 20, 20)",
        width = 2,
        shape = "spline"
      )
    )

  # Add train lines to the plot
  for (i in seq_along(trains)) {
    train_start_time <- train_data$train.start[i]
    train_end_time <- train_data$train.end[i]
    show_legend <- if (i == 1) TRUE else FALSE
    p <- p |>
      add_lines(
        x = c(train_start_time, train_end_time), y = c(0.98, 0.98),
        name = "Trains", line = list(color = "#009E73", width = 6),
        showlegend = show_legend, legendgroup = "Trains",
        hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2))
      )
  }

  # Add motif lines to the plot using motif_data
  for (i in seq_len(nrow(motif_data))) {
    motif_start <- motif_data$motif.start[i]
    motif_end <- motif_data$motif.end[i]
    show_legend <- if (i == 1) TRUE else FALSE
    p <- p |>
      add_lines(
        x = c(motif_start, motif_end), y = c(1, 1),
        name = "Motifs", line = list(color = "#0072B2", width = 6),
        showlegend = show_legend, legendgroup = "motifs",
        hoverinfo = "x", text = paste("Time:", round(c(motif_start, motif_end), 2))
      )
  }

  if (motif_seq) {
    # Add motif sequences
    for (i in seq_len(nrow(motif_seq_data))) {
      group_start <- motif_seq_data$group.start[i]
      group_end <- motif_seq_data$group.end[i]
      show_legend <- if (i == 1) TRUE else FALSE
      p <- p %>%
        add_lines(
          x = c(group_start, group_end), y = c(1.02, 1.02),
          name = "Motif Sequences", line = list(color = "#FF0000", width = 6),
          showlegend = show_legend, legendgroup = "motif.seqs",
          hoverinfo = "x", text = paste("Time:", round(c(group_start, group_end), 2))
        )
    }

  }


  # Add peak markers
  p <- p |>
    add_markers(
      x = ~ time_vector[peaks], y = ~ envelope_vector[peaks],
      name = "Peaks", marker = list(color = "#D55E00", size = 8),
      hoverinfo = "none"
    )

  p <- p |>
    layout(
      annotations = annotations,
      xaxis = list(
        title = list(text = "Time (s)", standoff = 10),
        ticklen = 5,
        automargin = TRUE,
        zeroline = FALSE,
        showline = TRUE
      ),
      yaxis = list(
        title = "Amplitude",
        rangemode = "tozero",
        ticklen = 5,
        showline = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.05,
        xanchor = "center",
        bgcolor = "rgba(0, 0, 0, 0)"
      ),
      margin = list(
        l = 70,
        r = 10,
        b = 50,
        t = 50
      )
    )

  # Add functionality to toggle the visibility of the Summary Statistics text box
  p <- htmlwidgets::onRender(p, "
   function(el, x) {
     el.on('plotly_restyle', function(d) {
      // We assume 'Summary Statistics' is the second trace (index 1)
      var traceVisible = x.data[0].visible;
      var annotations = x.layout.annotations;

      // Toggle annotation visibility based on the 'Summary Statistics' trace visibility
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

  p

  # Initialize return list
  return_list <- list(
    plot = p,
    summary_data = summary_data
  )

  if (motif_seq) {
    return_list$motif_seq_data <- motif_seq_data
  }

  return_list <- c(return_list, list(
    motif_data = motif_data,
    train_data = train_data,
    peak_data = peak_data,
    params = params
  ))

  # Return the final list
  return(return_list)

}
