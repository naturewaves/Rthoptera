#' Temporal and spectral statistics for HQ calls
#'
#' This function analyzes the acoustic characteristics of a wave object by
#' detecting sounds above a threshold in the envelope created with a mean
#' smoothing window, grouping them into motifs, and computing various metrics
#' related to their temporal and spectral structure.
#' An interactive plot visualizes the results, showing the envelope of the
#' signal, detected trains, motifs, and key statistics. The plot can be
#' downloaded as an HTML (interactive) or PNG (static) file.
#'
#' @param wave A Wave object containing the sound data.
#' @param specimen_id A character string representing the specimen ID.
#' @param msmooth_window An integer specifying the window size (in milliseconds)
#' for smoothing the envelope (default: 100).
#' @param msmooth_overlap An integer specifying the overlap (in %) for smoothing
#' the envelope. Default = 50.
#' @param upper_detection_threshold A numeric value representing the upper
#' amplitude detection threshold Default = 0.2.
#' @param lower_detection_threshold A numeric value representing the lower
#' amplitude detection threshold Default = 0.1.
#' @param min_train_dur A numeric value specifying the minimum duration (in
#' seconds) of a valid amplitude train. Default = 0.002.
#' @param max_train_gap A numeric value representing the maximum gap (in
#' seconds) allowed between two trains before considering them part of different
#' motifs. Default = 0.08.
#' @param motif_seq Logical. If TRUE, add an first-order aggregation of motifs. Default
#' value is FALSE.
#' @param max_motif_gap Numeric. Maximum allowed gap (in seconds) between motifs
#' to consider them part of the same group.
#'  @param norm_env A logical indicating whether to normalize the envelope
#'  Default = TRUE.
#'
#' @return A list with the following components:
#' \item{plot}{An interactive `plotly` plot showing the signal envelope,
#' detected trains, and motifs.}
#' \item{summary_data}{A tibble with summary statistics of the acoustic motifs.}
#' \item{train_data}{A tibble with details about the detected trains.}
#' \item{motif_data}{A tibble with details about the detected motifs.}
#' \item{params}{A tibble containing the input parameters.}
#' @export
#' @examples
#' \dontrun{
#' data(gryllus)
#' # result <- temporal_stats_hq(gryllus)
#' # result$plot
#' }
#' @importFrom dplyr mutate filter group_by summarize ungroup lead
#' @importFrom tibble tibble
#' @importFrom seewave env resamp
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom purrr map
#' @importFrom htmlwidgets onRender
call_stats_hq <- function(wave,
                          specimen_id = "",
                          msmooth_window = 100,
                          msmooth_overlap = 50,
                          upper_detection_threshold = 0.2,
                          lower_detection_threshold = 0.1,
                          min_train_dur = 0.002,
                          max_train_gap = 0.08,
                          motif_seq = TRUE,
                          max_motif_gap = 0.8,
                          norm_env = TRUE,
                          db_threshold = 20) {
  # Store input parameters in a tibble
  params <- tibble(
    specimen_id = specimen_id,
    msmooth_window = msmooth_window,
    msmooth_overlap = msmooth_overlap,
    max_train_gap = max_train_gap,
    upper_detection_threshold = upper_detection_threshold,
    lower_detection_threshold = lower_detection_threshold,
    norm_env = norm_env
  )

  msmooth_vec <- c(msmooth_window, msmooth_overlap)

  # Get envelope of the wave
  if (norm_env) {
    envelope_vector <- seewave::env(wave, msmooth = msmooth_vec,
                                    norm = TRUE, plot = FALSE)
  } else {
    envelope_vector <- seewave::env(wave, msmooth = msmooth_vec, plot = FALSE)
  }

  # Determine threshold based on maximum amplitude
  max_amplitude <- max(envelope_vector)
  amp_threshold <- max_amplitude * lower_detection_threshold
  min_amp_threshold <- max_amplitude * upper_detection_threshold

  # Create the time vector
  sample_rate <- wave@samp.rate
  time_vector <- seq(0, (length(wave@left) - 1) / sample_rate, length.out = length(envelope_vector)) # In seconds

  # Detect trains based on the amplitude lower_detection_threshold
  train_starts <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == 1)
  train_ends <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == -1) - 1

  # Initialize data storage
  trains <- list()

  for (i in seq_along(train_starts)) {
    train_start <- time_vector[train_starts[i]]
    train_end <- time_vector[train_ends[i]]
    train_max_amp <- max(envelope_vector[train_starts[i]:train_ends[i]])

    # Exclude trains below upper_detection_threshold
    if (train_max_amp < upper_detection_threshold) {
      next
    }

    # Append train to the list as a numeric vector
    trains <- append(trains, list(c(train_start, train_end)))
  }

  train_data <- tibble(
    specimen.id = rep(specimen_id, length(trains)),
    train.start = round(sapply(trains, `[[`, 1), 3),
    train.end = round(sapply(trains, `[[`, 2), 3),
    train.dur = round(sapply(trains, function(x) if (length(x) > 1) x[2] - x[1] else NA), 3)
  ) |>
    dplyr::filter(!is.na(train.dur) & train.dur > min_train_dur)

  # Calculate train.period and train.gap directly from train_data
  train_data <- train_data |>
    mutate(
      train.period = round(lead(train.start) - train.start, 3), # Period: next train.start - current train.start
      train.gap = round(lead(train.start) - train.end, 3) # Gap: next train.start - current train.end
    )

  # Calculate train.id and motif.id based on train.gap and max_train_gap
  train_data <- train_data |>
    mutate(
      motif.id = cumsum(c(TRUE, train.gap[-n()] > max_train_gap)),
      train.id = sequence(rle(cumsum(c(TRUE, train.gap[-n()] > max_train_gap)))$lengths)
    )


  # Add Spectral Statistics for each train
  train_data <- train_data  |>
    rowwise() |>
    mutate(
      spectral_stats = list(
        tryCatch(
          {

            spec1 <- seewave::meanspec(wave, wl = 128,
                              from = train.start,
                              to = train.end,
                              dB = NULL, plot = FALSE)
            sp.ent <- seewave::sh(spec1)
            sp.flat <- seewave::sfm(spec1)
            rm(spec1)

            # Calculate meanspec for the train
            spec <- seewave::meanspec(wave,
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
            tibble(peak.freq = NA, low.freq = NA, high.freq = NA, bandw = NA,
                   sp.exc = NA, sp.ene = NA, sp.ent = NA, sp.flat = NA)
          }
        )
      )
    )  |>
    ungroup()  |>
    # Expand the spectral stats into individual columns
    unnest_wider(spectral_stats)

  # Summarize motif data
  motif_data <- train_data |>
    group_by(motif.id) |>
    reframe(
      motif.start = min(train.start),
      motif.end = max(train.end),
      motif.dur = round(motif.end - motif.start, 3),
      n.trains = n(),
      train.rate = round((n.trains - 1) / motif.dur, 3),
      duty.cycle = round(sum(train.dur) / motif.dur * 100, 1),
      peak.freq.mean = round(mean(train_data$peak.freq, na.rm = TRUE), 3),
      # peak.freq.var = round(stats::var(train_data$peak.freq, na.rm = TRUE), 3),
      peak.freq.sd = round(sd(train_data$peak.freq, na.rm = TRUE), 3),
      low.freq.mean = round(mean(train_data$low.freq, na.rm = TRUE),3),
      # low.freq.var = round(var(train_data$low.freq, na.rm = TRUE), 3),
      low.freq.sd = round(sd(train_data$low.freq, na.rm = TRUE), 3),
      high.freq.mean = round(mean(train_data$high.freq, na.rm = TRUE),3),
      # high.freq.var = round(var(train_data$high.freq, na.rm = TRUE), 3),
      high.freq.sd = round(sd(train_data$high.freq, na.rm = TRUE), 3),
      bandw.mean = round(mean(train_data$bandw, na.rm = TRUE),3),
      # bandw.var = round(var(train_data$bandw, na.rm = TRUE), 3),
      bandw.sd = round(sd(train_data$bandw, na.rm = TRUE), 3),
      sp.exc.mean = round(mean(train_data$sp.exc, na.rm = TRUE), 3),
      # sp.exc.var = round(var(sp.exc, na.rm = TRUE), 3),
      sp.exc.sd = round(sd(train_data$sp.exc, na.rm = TRUE), 3),
      sp.ene.mean = round(mean(train_data$sp.ene, na.rm = TRUE), 3),
      # sp.ene.var = round(var(sp.ene, na.rm = TRUE), 3),
      sp.ene.sd = round(sd(train_data$sp.ene, na.rm = TRUE), 3),
      sp.ent.mean = round(mean(train_data$sp.ent, na.rm = TRUE), 3),
      # sp.ent.var = round(var(sp.ent, na.rm = TRUE), 3),
      sp.ent.sd = round(sd(train_data$sp.ent, na.rm = TRUE), 3),
      sp.flat.mean = round(mean(train_data$sp.flat, na.rm = TRUE), 3),
      # sp.flat.var = round(var(sp.flat, na.rm = TRUE), 3),
      sp.flat.sd = round(sd(train_data$sp.flat, na.rm = TRUE), 3)
    ) |>
    ungroup()

  # Add proportions and PCI
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



  motif_data <- motif_data |>
    select(motif.id, n.trains, everything())

  # Add proportions and complexity metrics
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
              dplyr::nth(i)
            gap_end <- train_data |>
              filter(motif.id == eid) |>
              pull(train.start) |>
              dplyr::nth(i + 1)

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
    select(specimen.id, everything(), -proportions, proportions)


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





  # Prepare summary data
  summary_data <- tibble(
    specimen.id = base::unique(train_data$specimen.id),
    n.motifs = nrow(motif_data),
    pci.mean = round(mean(motif_data$pci, na.rm = TRUE), 3),
    pci.sd = round(sd(motif_data$pci, na.rm = TRUE), 3),
    duty.cycle.mean = round(mean(motif_data$duty.cycle, na.rm = TRUE), 1),
    duty.cycle.sd = round(sd(motif_data$duty.cycle, na.rm = TRUE), 1),
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

  annotations <- list(
    list(
      x = 0.01,
      y = 0.01,
      xref = "paper",
      yref = "paper",
      text = paste(
        "<b> Summary Statistics</b>",
        "<br> N. motifs: ", summary_data$n.motifs,
        "<br> Duty cycle: ", summary_data$duty.cycle.mean, "%",
        "<br> Trains/motif: ", summary_data$n.trains.mean,
        "<br> Motif dur.: ", summary_data$motif.dur.mean, "s",
        "<br> Train dur.: ", summary_data$train.dur.mean, "s",
        "<br> Train gap: ", summary_data$gap.dur.mean, "s",
        "<br> Train rate: ", summary_data$train.rate.mean, "Hz",
        "<br> Mean entropy: ", summary_data$entropy.mean,
        "<br> Mean PCI: ", summary_data$pci.mean
      ),
      showarrow = FALSE,
      font = list(size = 12),
      align = "left",
      bgcolor = "rgba(255, 255, 255, 0.8)",
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
      x = ~time_vector, y = ~envelope_vector,
      name = "Envelope",
      hoverinfo = "none",
      line = list(
        color = "rgb(20, 20, 20)",
        width = 2,
        shape = "spline"
      )
    ) |>
    add_lines(
      x = c(min(time_vector), max(time_vector)), y = c(lower_detection_threshold, lower_detection_threshold),
      name = "Lower Threshold", line = list(color = "#D55E00", dash = "dash"), showlegend = TRUE
    )

  # Add train lines to the plot
  for (i in seq_len(nrow(train_data))) {
    train_start_time <- train_data$train.start[i]
    train_end_time <- train_data$train.end[i]
    show_legend <- if (i == 1) TRUE else FALSE
    p <- p |>
      add_lines(
        x = c(train_start_time, train_end_time), y = c(0.98, 0.98),
        name = "Trains", line = list(color = "#009E73", width = 6),
        showlegend = show_legend, legendgroup = "trains",
        hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2))
      )
  }

  # Add motif lines to the plot
  for (i in seq_len(nrow(motif_data))) {
    motif_start_time <- motif_data$motif.start[i]
    motif_end_time <- motif_data$motif.end[i]
    show_legend <- if (i == 1) TRUE else FALSE
    p <- p |>
      add_lines(
        x = c(motif_start_time, motif_end_time), y = c(1, 1),
        name = "Motifs", line = list(color = "#0072B2", width = 6),
        showlegend = show_legend, legendgroup = "Motifs",
        hoverinfo = "x", text = paste("Motif:", i)
      )
  }

  if (motif_seq) {
    # Add motif groups
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


  p <- p |>
    layout(
      annotations = annotations,
      title = summary_data$specimen.id,
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

  return_list <- list(
    plot = p,
    summary_data = summary_data
    )

  if (motif_seq) {
    return_list$motif_seq_data <- motif_seq_data
  }


  return_list <- c(return_list, list(
    train_data = train_data,
    motif_data = motif_data,
    params = params
  ))

  return(return_list)
}
