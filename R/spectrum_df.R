#' Spectrum Table (Tibble, Data Frame)
#'
#' This function generates a data frame containing the frequency and amplitude
#' of a wave object using the mean spectrum method.
#' The function leverages the `meanspec` function from the `seewave` package to
#' compute the spectrum.
#'
#' @param wave An object of class `Wave` containing the audio data to be
#' analyzed.
#' @param freq_res The frequency resolution to be used for the frequency
#' spectrum analysis. Use this argument consistently for standardized
#' measurements across recordings with different sampling rate. Default is
#' 10 Hz per frequency bin.
#' @param fun Character string indicating the summary function to be applied
#' for computing the mean spectrum. Default is `'mean'`. Other options include
#' `'median'`, `'sd'` (standard deviation), or `'var'` (variance).
#' @param wn Window filtering function. Choices are:  bartlett, blackman,
#' flattop, hamming, hanning, or rectangle. Default is `hanning`.
#' @param verbose Logical. If TRUE, parameter info is printed in the console.
#' @param ... Other arguments passed to meanspec() from the seewave package.
#'
#' @return A list with two tibbles. The first tibble "spec_df" with two columns:
#' `frequency` and `amplitude`, and the second tibble "params_df" contains all
#' the relevant parameters used to create the spectrum.
#' The `frequency` column contains the frequencies in Hertz (Hz), and
#' the `amplitude` column contains the corresponding amplitude values.
#'
#' @export
#' @import fftw
#' @importFrom tibble tibble
#' @importFrom seewave meanspec
#'
#' @examples
#'  \dontrun{
#' spec_df <- meanspec_df(wave, from = 0, to = 5, wl = 1024, fun = 'mean')
#' }
spectrum_df <- function(wave,
                        freq_res = 10,
                        fun = "mean",
                        wn = "hanning",
                        verbose = FALSE,...) {

  # Determine the window length needed to get the desired frequency resolution
  wl <- wave@samp.rate / freq_res

  if (wl %% 2 == 1) {
    wl <- wl + 1
  }

  # Store the parameters in a new tibble
  params <- tibble(
    srate = wave@samp.rate,
    freq.res = freq_res,
    wl = wl,
    wn = wn,
    fun = fun
  )

  if (verbose) {
    cat(paste0("Sampling rate: ", wave@samp.rate, " Hz",
               "\nFrequency resolution: ", freq_res, " Hz per bin.",
               "\nWindow length: ", wl, " samples.",
               "\nWindow type: ", wn, ".",
               "\nSummary function: ", fun, ".\n"))
  }

  full_spec <- seewave::meanspec(wave,
                                 wl = wl,
                                 FUN = fun,
                                 fftw = TRUE,
                                 plot = FALSE,
                                 ...)

  full_spec_df <- tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])

  return(list(spec_df = full_spec_df, params_df = params))
}
