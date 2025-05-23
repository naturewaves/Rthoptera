% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrum_ggplot.R
\name{spectrum_ggplot}
\alias{spectrum_ggplot}
\title{Power Spectrum with ggplot2}
\usage{
spectrum_ggplot(
  wave,
  auto_wl = TRUE,
  wl = NULL,
  ovlp = 75,
  ymin = -100,
  y_breaks = c(-40, -20, 0),
  x_breaks = 6,
  y_position = "left",
  x_position = "bottom",
  flip = FALSE,
  color_db = "#4d4d4d",
  color_linear = "#000000",
  color_carrier = "#1e90ff",
  color_threshold = "#228b22",
  color_bandwidth = "#228b22",
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
  show_lines = FALSE
)
}
\arguments{
\item{wave}{A Wave object (from \code{tuneR} package), representing the sound
waveform.}

\item{auto_wl}{Logical. If \code{TRUE}, the window length (\code{wl}) is automatically
calculated based on the sampling rate of the waveform.}

\item{wl}{Numeric. Window length for the FFT. Ignored if \code{auto_wl} is \code{TRUE}.}

\item{ovlp}{Numeric. Overlap between windows in percentage (0 to 100).
Default is 75.}

\item{ymin}{Numeric. Minimum value for the y-axis (used when plotting
in dB scale).}

\item{y_breaks}{Numeric vector. Break points for the y-axis in the dB
scale plot.}

\item{x_breaks}{Integer. Number of breaks for the x-axis (frequency).}

\item{y_position}{Character. Position of the y-axis: "left" or "right".}

\item{x_position}{Character. Position of the x-axis: "bottom" or "top".}

\item{flip}{Logical. If \code{TRUE}, flips the x and y axes.}

\item{color_db}{Character. Color for the dB scale plot.}

\item{color_linear}{Character. Color for the linear scale plot.}

\item{color_carrier}{Character. Color for the carrier frequency line.}

\item{color_threshold}{Character. Color for the threshold line.}

\item{color_bandwidth}{Character. Color for the bandwidth lines.}

\item{fun}{Character. Summary function for calculating the power spectrum,
default is "mean".}

\item{wn}{Character. Windowing function for the FFT. Default is "blackman".}

\item{show_x_title}{Logical. If \code{TRUE}, displays the x-axis title.}

\item{show_y_title}{Logical. If \code{TRUE}, displays the y-axis title.}

\item{add_params}{Logical. If \code{TRUE}, adds parameter information
(window size, overlap, etc.) to the plot.}

\item{add_summary}{Logical. If \code{TRUE}, adds spectral summary statistics
(entropy, flatness, carrier frequency, bandwidth) to the plot.}

\item{plot_title}{Character. Title for the plot.}

\item{italic_title}{Logical. If \code{TRUE}, italicizes the plot title.}

\item{fmin}{Numeric. Minimum frequency to display (in kHz).}

\item{fmax}{Numeric. Maximum frequency to display (in kHz). If \code{NULL},
defaults to half the sampling rate (Nyquist frequency).}

\item{linewidth}{Numeric. Line width for the carrier and bandwidth lines.}

\item{total_bandwidth}{Logical. If \code{TRUE}, calculates the total bandwidth
using a threshold of 0.1.}

\item{show_lines}{Logical. If \code{TRUE}, displays lines for the carrier
frequency and bandwidth.}
}
\value{
A \code{ggplot} object for the power spectrum plot.
}
\description{
This function generates a power spectrum plot using \code{ggplot2} based on the
provided waveform object (\code{wave}). The plot can display normalized amplitude
values on both dB and linear scales, and includes options for customizing the
display of the carrier frequency, bandwidth, and other spectral details.
}
\examples{
\dontrun{
spectrum_ggplot(coryphoda)
}
}
