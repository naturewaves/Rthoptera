#' Calculate the mode(s) of a vector
#'
#' @param x A vector (numeric, character, factor, etc.)
#' @param na.rm Logical indicating whether to remove NA values (default: TRUE)
#' @return A vector containing the mode(s). If multiple modes exist, returns all of them.
#'         Returns NA if input is empty or all values are NA (when na.rm = TRUE).
#' @export
#' @examples
#' get_mode(c(1, 2, 2, 3, 3, 4))  # Returns c(2, 3)
#' get_mode(c("a", "b", "b", "c")) # Returns "b"
#' get_mode(rep(NA, 5))            # Returns NA
get_mode <- function(x, na.rm = TRUE) {
  # Handle NA values
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # If all values are NA (and na.rm = FALSE) or vector is empty, return NA
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }

  # Calculate frequencies
  freq_table <- table(x)
  max_freq <- max(freq_table)

  # Get all values with maximum frequency
  modes <- names(freq_table[freq_table == max_freq])

  # Convert back to original type if numeric
  if (is.numeric(x)) {
    modes <- as.numeric(modes)
  }

  # For factors, return factor with original levels
  if (is.factor(x)) {
    modes <- factor(modes, levels = levels(x))
  }

  # Return single value or vector as appropriate
  if (length(modes) == 1) {
    return(modes[1])
  } else {
    return(modes)
  }
}
