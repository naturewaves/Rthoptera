#' Denoise a Wave for temporal measurements
#'
#' @param wave a Wave object.
#' @param filter Character. Select a bandpass filter. Options are 'fir' (Finite
#' Impulse Response);'bwf' (Butterworth); and 'freq' (Frequency filter).
#' @param min Numeric. The minimum frequency for the bandpass filter.
#' @param max Numeric. The maximum frequency for the bandpass filter.
#' @param gate Numeric (double). The proportion of the maximum amplitude to use
#' as a threshold for the noise gate. The values below this threshold are
#' clipped to zero.
#' @param oscillo Logical. If TRUE (default), a simple oscillogram of the
#' output wave is plotted.
#'
#' @return a Wave object
#' @export
#' @importFrom seewave ffilter fir bwfilter
#'
#' @details
#' This function was developed as a preprocessing step for temporal measurements
#' only. DO NOT conduct spectral analysis on objects returned by this function.
#'
#' @examples
#' \dontrun{
#' library(RthopteraSounds)
#' data(Coryphoda)
#' clean_wave(Coryphoda)
#' }
clean_wave <- function(wave,
                       filter = "fir",
                       min = NULL,
                       max = NULL,
                       gate = 0.05,
                       oscillo = TRUE){

  if (is.null(min)) {
    min = 0
  }

  if (is.null(max)) {
    # Set to Nyquist as default
    max = wave@samp.rate / 2
  }

  from = min * 1000
  to = max * 1000

  if (filter == "fir") {
    filtered_wave <- fir(wave, from = from, to = to,
                         output = "Wave")
  } else if (filter == "bwf"){
    filtered_wave <- bwfilter(wave, from = from, to = to,
                         output = "Wave")
  }else if (filter == "freq"){
    filtered_wave <- ffilter(wave, from = from, to = to,
                              output = "Wave")
  } else {
    stop("Invalid filter input. Options are 'fir'
    (Finite Impulse Response);'bwf' (Butterworth);
         and 'freq' (Frequency filter). \n")
  }

  clean_wave <- noise_gate(filtered_wave,
                           threshold = gate,
                           oscillo = oscillo)

  return(clean_wave)

}
