#' Rthoptera: A Shiny-powered R package for standard insect bioacoustics
#'
#' @description
#' Rthoptera provides interactive Shiny applications for standard analysis of
#' insect sounds. The package is intended to be used on high signal-to-noise
#' recordings, helping researchers make standardized measurements and plots to
#' support the scientific description of the "acoustic type". We define
#' "acoustic type" as the first description of the calling song of a species,
#' which should be accompanied by a high-quality recording and the catalog
#' number of the voucher specimen. For the convenience of users, we have
#' included functions for pre-processing audio files, which are already
#' available in other packages (e.g., tuneR, seewave), but here they are
#' interactive (Shiny apps). Most of the plotting functions are based on the
#' seewave package, with the convenience of presets and automatic parameter
#' selection (e.g., window length for the spectrograms) to ensure clarity in
#' any Wave regardless of the recording settings. Most of these plots also have
#' interactive versions which can be saved as HTML documents.
#'
#' @details
#' Key features include:
#' \itemize{
#'   \item Functions and apps for processing wave data
#'   \item Functions and apps for executing spectral and temporal analyses with
#'   emphasis on Orthoptera.
#' }
#'
#' @section Package options:
#' \describe{
#'   \item{`Rthoptera.verbose`}{Logical. Set package-wide verbosity (default: `TRUE`)}
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/naturewaves/Rthoptera}
#'   \item Report bugs at \url{https://github.com/naturewaves/Rthoptera/issues}
#' }
#'
#' @keywords internal
"_PACKAGE"
