#' Apply song_stats functions to Wave Objects in Parallel
#'
#' Processes multiple `Wave` objects in parallel using `song_stats_lq()`, suppresses plots,
#' and combines results into a structured list of tables.
#'
#' @param wave_objects A list of `Wave` objects or a character vector of object names.
#' @param cores Number of CPU cores to use (default: `parallel::detectCores() - 1`).
#' @param q Character. Either "low" to use the `song_stats_lq()` function
#'  or "high" to call the `song_stats_hq()` function. 
#' @param ... Additional arguments passed to `song_stats_lq()` or `song_stats_hq()`.
#' @return A list of merged tibbles (one per output table).
#'
#' @examples
#' \dontrun{
#' library(tuneR)
#' wave1 <- sine(440)
#' wave2 <- noise()
#' results <- parallel_song_stats(c("wave1", "wave2"), cores = 2)
#' }
#'
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply
#' @importFrom dplyr bind_rows
#' @export
batch_song_stats <- function(wave_objects, cores = NULL, q,...) {
  # Load required packages in workers
  require(tuneR)
  require(dplyr)
  
  if (!q %in% c("low", "high")){
    stop("'q' should be either 'low' or 'high.")
  }
  
  # Convert character names to actual objects if needed
  if (is.character(wave_objects)) {
    wave_objects <- mget(wave_objects, envir = .GlobalEnv, ifnotfound = list(NULL))
    wave_objects <- Filter(Negate(is.null), wave_objects)
  }
  
  # Set up parallel cluster
  if (is.null(cores)) cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))
  
  if (q == "low"){
    # Export needed functions/variables
    parallel::clusterExport(cl, c("song_stats_lq"), envir = environment())
    
    # Process in parallel (suppress plots)
    results <- parallel::parLapply(cl, wave_objects, function(wave) {
      res <- song_stats_lq(wave, ...)
      res$plot <- NULL  
      res
    })
    
    # Combine results by table type
    merged <- list(
      summary_data = lapply(results, `[[`, "summary_data") %>% dplyr::bind_rows(.id = "wave"),
      motif_seq_data = lapply(results, `[[`, "motif_seq_data") %>% dplyr::bind_rows(.id = "wave"),
      motif_data = lapply(results, `[[`, "motif_data") %>% dplyr::bind_rows(.id = "wave"),
      train_data = lapply(results, `[[`, "train_data") %>% dplyr::bind_rows(.id = "wave"),
      peak_data = lapply(results, `[[`, "peak_data") %>% dplyr::bind_rows(.id = "wave"),
      params = lapply(results, `[[`, "params") %>% dplyr::bind_rows(.id = "wave")
    )
    
  } else {
    # Export needed functions/variables
    parallel::clusterExport(cl, c("song_stats_hq"), envir = environment())
    
    # Process in parallel (suppress plots)
    results <- parallel::parLapply(cl, wave_objects, function(wave) {
      res <- song_stats_hq(wave, ...)
      res$plot <- NULL  
      res
    })
    
    # Combine results by table type
    merged <- list(
      summary_data = lapply(results, `[[`, "summary_data") %>% dplyr::bind_rows(.id = "wave"),
      motif_seq_data = lapply(results, `[[`, "motif_seq_data") %>% dplyr::bind_rows(.id = "wave"),
      motif_data = lapply(results, `[[`, "motif_data") %>% dplyr::bind_rows(.id = "wave"),
      train_data = lapply(results, `[[`, "train_data") %>% dplyr::bind_rows(.id = "wave"),
      # peak_data = lapply(results, `[[`, "peak_data") %>% dplyr::bind_rows(.id = "wave"),
      params = lapply(results, `[[`, "params") %>% dplyr::bind_rows(.id = "wave")
    )
    
    
  }

  
  return(merged)
}
