#' List and Summarize Wave Objects in the Environment
#'
#' Scans the global environment for objects of class `Wave` (from the `tuneR` package)
#' and returns a summary table with their key attributes, including duration, sample rate,
#' bit depth, and channel configuration.
#'
#' @return A `data.frame` with one row per `Wave` object and the following columns:
#' \describe{
#'   \item{Name}{Name of the object in the environment (character)}
#'   \item{Duration}{Length in seconds (numeric, rounded to 2 decimal places)}
#'   \item{SampleRate}{Sampling rate in Hz (integer)}
#'   \item{BitDepth}{Bit depth (integer, typically 16 or 24)}
#'   \item{Channels}{"Mono" or "Stereo" (character)}
#'   \item{Samples}{Total number of samples (integer)}
#' }
#' Returns `NULL` (invisibly) with a message if no `Wave` objects are found.
#'
#' @examples
#' \dontrun{
#' # Create example Wave objects
#' library(tuneR)
#' sine_wave <- sine(440, duration = 1, samp.rate = 44100, bit = 16)
#' noise_wave <- noise(duration = 0.5, samp.rate = 48000, bit = 24)
#'
#' # Get summary table
#' wave_info()
#' }
#'
#' @export
#' @importFrom tuneR "Wave"
wave_info <- function() {
  # Get all Wave objects in the environment
  wave_objects <- ls(envir = .GlobalEnv)[
    sapply(ls(envir = .GlobalEnv), function(x) {
      inherits(get(x, envir = .GlobalEnv), "Wave")
    })
  ]

  if (length(wave_objects) == 0) {
    message("No Wave objects found in the environment.")
    return(NULL)
  }

  # Extract attributes for each Wave object
  wave_info <- lapply(wave_objects, function(obj_name) {
    obj <- get(obj_name, envir = .GlobalEnv)
    data.frame(
      Name = obj_name,
      Duration = round(length(obj@left) / obj@samp.rate, 2),
      SampleRate = obj@samp.rate,
      BitDepth = obj@bit,
      Channels = ifelse(obj@stereo, "Stereo", "Mono"),
      Samples = length(obj@left),
      stringsAsFactors = FALSE
    )
  })

  # Combine into a single data frame
  result <- do.call(rbind, wave_info)
  return(result)
}
