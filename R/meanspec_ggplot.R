#' Plot a Mean Power Spectrum using ggplot2
#'
#' @param wave A Wave object.
#' @param scale_type Character. Scale for the amplitude. Either "dB"
#' (Full Scale) or  "linear".
#' @param auto_wl Logical. Whether to use an automatic procedure to obtain a
#' standard frequency resolution.
#' @param wl Numeric. Number of samples for the window length.
#'
#' @return A plot.
#' @keywords internal
#' @noRd
#'
#' @importFrom ggplot2 ggplot aes theme_minimal geom_ribbon scale_y_continuous
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' data(coryphoda)
#' meanspec_ggplot(coryphoda)
#' }
meanspec_ggplot <- function(wave,
                            scale_type = "dB",
                            auto_wl = TRUE,
                            wl = NULL,
                            overlap = 95) {
  if (auto_wl) {
    wl <- round(wave@samp.rate * sqrt(seewave::duration(wave)) * 20e-4)
  } else {
    wl <- wl
  }

  if (wl %% 2 != 0) {
    wl <- wl + 1
  }

  db_value <- if (scale_type == "dB") "max0" else NULL
  mean_spectrum <- seewave::meanspec(wave,
    f = wave@samp.rate,
    wl = wl,
    ovlp = overlap,
    dB = db_value,
    plot = FALSE
  )
  meanspec_data <- data.frame(
    freq = mean_spectrum[, 1],
    mean_amp = mean_spectrum[, 2]
  )


  spectrum_plot <- meanspec_data |>
    ggplot(aes(x = .data$freq, y = .data$mean_amp)) +
    theme_minimal(base_size = 15)

  if (scale_type == "dB") {
    spectrum_plot <- spectrum_plot +
      geom_ribbon(aes(x = .data$freq, ymin = -50, ymax = .data$mean_amp),
        fill = "black"
      ) +
      scale_y_continuous(
        breaks = c(-40, -20, 0),
        limits = c(-50, 0),
        expand = expansion(mult = c(0, .1)),
        position = "right"
      )
  } else {
    spectrum_plot <- spectrum_plot +
      geom_ribbon(aes(x = .data$freq, ymin = 0, ymax = .data$mean_amp),
        fill = "black"
      ) +
      scale_y_continuous(
        breaks = c(0, 0.5, 1),
        expand = expansion(mult = c(0, .1)),
        position = "right",
        labels =
          function(x) {
            ifelse(x == 0 | x == 1,
              as.character(as.integer(x)),
              as.character(x)
            )
          }
      )
  }


  if (scale_type == "dB") {
    title <- "dB"
  } else if (scale_type == "linear") {
    title <- "Amplitude"
  }
  spectrum_plot <- spectrum_plot +
    scale_x_continuous(
      expand = c(0, 0),
      position = "top",
      breaks = scales::breaks_pretty(),
      labels = scales::label_number(zero.print = "")
    ) +
    theme_bw() +
    theme(
      plot.margin = margin(t = 0, r = 10, b = 10, l = 0, unit = "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.ticks.y = element_line(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = title,
      title = ""
    ) +
    coord_flip()

  return(spectrum_plot)
}
