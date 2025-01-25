#' Apply a noise gate to a Wave
#'
#' @param wave A Wave object
#' @param threshold Numeric. The amplitude threshold to select the clipping value,
#' as a proportion of the maximum amplitude in the Wave.
#' @param oscillo Logical. If TRUE (default) an oscillogram of the modified Wave
#' is printed.
#'
#' @return A Wave object.
#' @noRd
#'
#' @importFrom seewave oscillo
#'
#' @examples
#' \dontrun{
#' library(RthopteraSounds)
#' data(Coryphoda)
#' clean_wave(Coryphoda)
#' }
noise_gate <- function(wave, threshold = 0.05, oscillo = TRUE) {
  # Find the maximum absolute value in the wave data
  max_val <- max(abs(wave@left))
  effective_threshold <- max_val * threshold

  # Apply threshold
  wave@left[abs(wave@left) < effective_threshold] <- 0

  # If stereo
  if (!is.null(wave@right)) {
    wave@right[abs(wave@right) < effective_threshold] <- 0
  }
  if (oscillo) {
    p <- oscillogram_ggplot(wave, save = FALSE)
    plot(p)
  }
  return(wave)
}





