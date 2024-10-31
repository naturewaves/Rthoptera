#' Merge Multiple Wave Objects into One
#'
#' This function takes a list of `Wave` objects and merges them over time
#' into a single `Wave` object.
#' It concatenates the left and right channels (if stereo) and normalizes the
#' resulting `Wave` object.
#'
#' @param wave_list A list of `Wave` objects to be merged. The objects should be
#' compatible in terms of sample rate and number of channels (mono or stereo).
#'
#' @return A merged `Wave` object with the same sample rate as the input
#' objects. The channels are concatenated over time.
#' Returns `NULL` if the input list is empty.
#' @noRd
#'
#' @examples
#' library(tuneR)
#' wave1 <- sine(440, duration = 1, samp.rate = 44100)
#' wave2 <- sine(880, duration = 1, samp.rate = 44100)
#' merged_wave <- merge_waves(list(wave1, wave2))
#' play(merged_wave)
merge_waves <- function(wave_list) {
  if (length(wave_list) == 0) {
    return(NULL)
  } # If no waves are selected, return NULL

  # Start with the first wave in the list
  merged_wave <- wave_list[[1]]

  # Iterate through the rest of the waves and concatenate them over time
  for (i in 2:length(wave_list)) {
    current_wave <- wave_list[[i]]

    # Concatenate the left channels of the waves (assuming mono or stereo)
    merged_wave@left <- c(merged_wave@left, current_wave@left)

    # If stereo, concatenate the right channels as well
    if (merged_wave@stereo) {
      merged_wave@right <- c(merged_wave@right, current_wave@right)
    }

    merged_wave <- tuneR::normalize(merged_wave,
      unit = "24",
      pcm = TRUE,
      center = TRUE
    )
  }

  return(merged_wave)
}
