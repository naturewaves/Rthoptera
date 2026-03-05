#' Query Xeno-Canto recordings by taxonomic rank using GBIF backbone
#'
#' \code{query_xenocanto_taxon} retrieves Xeno-Canto recording metadata for all
#' species belonging to a given taxon (e.g., family, order, genus) by first
#' resolving the species list from the GBIF taxonomic backbone and then querying
#' Xeno-Canto for each species. Optionally downloads the audio files.
#'
#' @param taxon Character string. The name of the taxon to search (e.g.,
#'   \code{"Tettigoniidae"}, \code{"Orthoptera"}, \code{"Conocephalus"}).
#' @param rank Character string. The taxonomic rank of \code{taxon}. One of
#'   \code{"order"}, \code{"family"}, \code{"genus"}, \code{"subfamily"},
#'   \code{"tribe"}, or \code{"class"}. Default is \code{"family"}.
#' @param country Character string (optional). Two-letter ISO country code to
#'   restrict GBIF species lookup (e.g., \code{"CL"} for Chile, \code{"PE"}
#'   for Peru). Default is \code{NULL} (global).
#' @param xc_query_extra Character string (optional). Additional Xeno-Canto
#'   advanced query tags appended to each species query, e.g.
#'   \code{'type:"song"'} or \code{'cnt:"Chile"'}. Default is \code{NULL}.
#' @param download Logical. If \code{TRUE}, audio files are downloaded after
#'   querying. Default is \code{FALSE}.
#' @param download_path Character string. Directory path where audio files will
#'   be saved when \code{download = TRUE}. Will be created if it does not exist.
#'   Default is \code{"."} (current working directory).
#' @param quality_filter Character vector. Only download recordings whose
#'   quality rating is in this set. Xeno-Canto quality ratings are \code{"A"}
#'   (best) through \code{"E"} (worst) plus \code{"no score"}. Set to
#'   \code{NULL} to download all qualities. Default is \code{c("A", "B")}.
#' @param subdir_by_species Logical. If \code{TRUE}, files are saved in
#'   per-species subdirectories under \code{download_path} (e.g.
#'   \code{download_path/Genus_species/}). Default is \code{TRUE}.
#' @param overwrite Logical. If \code{FALSE}, files that already exist on disk
#'   are skipped. Default is \code{FALSE}.
#' @param download_delay Numeric. Seconds to wait between download requests
#'   to avoid hammering the server. Default is \code{0.3}.
#' @param api_key Character string. Xeno-Canto API v3 key. Defaults to the
#'   \code{xc_api_key} environment variable. Get yours at
#'   \href{https://xeno-canto.org/account}{https://xeno-canto.org/account}.
#' @param query_delay Numeric. Seconds to wait between Xeno-Canto metadata API
#'   requests. Default is \code{0.5}.
#' @param cores Integer. Number of cores for parallel processing within each
#'   \code{query_xenocanto} call. Default is \code{1}.
#' @param pb Logical. Show progress bars. Default is \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default is \code{TRUE}.
#' @param all_data Logical. If \code{TRUE}, all metadata columns are returned
#'   from Xeno-Canto. Default is \code{FALSE}.
#' @param raw_data Logical. If \code{TRUE}, raw unformatted data is returned.
#'   Default is \code{FALSE}.
#' @param gbif_limit Integer. Maximum number of records to retrieve from GBIF
#'   per page request (max 1000). Default is \code{1000}.
#'
#' @return A data frame combining Xeno-Canto metadata for all species found
#'   under the queried taxon. A column \code{queried_taxon} records the matched
#'   taxon name. When \code{download = TRUE}, an additional column
#'   \code{local_path} is appended with the absolute path of each downloaded
#'   file (\code{NA} if skipped or failed). Returns \code{NULL} invisibly if no
#'   records are found.
#'
#' @details
#' The function proceeds in up to four steps:
#' \enumerate{
#'   \item \strong{GBIF backbone match}: Resolves the taxon name and rank to a
#'     stable GBIF usage key via \code{/v1/species/match}.
#'   \item \strong{Species list retrieval}: Recursively walks
#'     \code{/v1/species/{key}/children}, paginating through the full
#'     taxonomic tree until species-rank leaves are reached. Only
#'     \code{ACCEPTED} names are retained to avoid synonym duplication. If
#'     \code{country} is supplied, each species is cross-checked against GBIF
#'     occurrence records for that country before querying XC.
#'   \item \strong{Xeno-Canto metadata queries}: Calls
#'     \code{\link{query_xenocanto}} for each species using the \code{sp:} tag,
#'     optionally appending \code{xc_query_extra}. Results are bound into one
#'     data frame.
#'   \item \strong{Audio download} (only when \code{download = TRUE}):
#'     Downloads each recording using the \code{file_url} column. Files are
#'     named \code{<Genus_species>_<XC_id>.<ext>} and optionally organised into
#'     per-species subdirectories. Already-existing files are skipped unless
#'     \code{overwrite = TRUE}.
#' }
#'
#' @seealso \code{\link{query_xenocanto}}
#'
#' @examples
#' \dontrun{
#' Sys.setenv(xc_api_key = "YOUR_KEY_HERE")
#'
#' # Metadata only - all Chilean Tettigoniidae
#' tetti <- query_xenocanto_taxon("Tettigoniidae", rank = "family", country = "CL")
#'
#' # Metadata + download A/B quality into organised subdirectories
#' tetti <- query_xenocanto_taxon(
#'   taxon          = "Tettigoniidae",
#'   rank           = "family",
#'   country        = "CL",
#'   download       = TRUE,
#'   download_path  = "~/xc_audio/tettigoniidae_CL",
#'   quality_filter = c("A", "B")
#' )
#'
#' # Songs only, all qualities, flat directory layout
#' conoceph <- query_xenocanto_taxon(
#'   taxon             = "Conocephalus",
#'   rank              = "genus",
#'   xc_query_extra    = 'type:"song"',
#'   download          = TRUE,
#'   download_path     = "~/xc_audio/conocephalus_songs",
#'   quality_filter    = NULL,
#'   subdir_by_species = FALSE
#' )
#'
#' # Download only quality A, do not re-download files already on disk
#' orth <- query_xenocanto_taxon(
#'   taxon          = "Orthoptera",
#'   rank           = "order",
#'   country        = "CL",
#'   download       = TRUE,
#'   download_path  = "~/xc_audio/orthoptera_CL",
#'   quality_filter = "A",
#'   overwrite      = FALSE
#' )
#' }
#'
#' @author Based on \code{query_xenocanto} by Marcelo Araya-Salas.
#'   Extended by Fran Lerida.

query_xenocanto_taxon <- function(
    taxon,
    rank               = "family",
    country            = NULL,
    xc_query_extra     = NULL,
    # --- download params ---
    download           = FALSE,
    download_path      = ".",
    quality_filter     = c("A", "B"),
    subdir_by_species  = TRUE,
    overwrite          = FALSE,
    download_delay     = 0.3,
    # --- query / session params ---
    api_key            = Sys.getenv("xc_api_key"),
    query_delay        = 0.5,
    cores              = getOption("mc.cores", 1),
    pb                 = getOption("suwo_pb", TRUE),
    verbose            = getOption("suwo_verbose", TRUE),
    all_data           = getOption("suwo_all_data", FALSE),
    raw_data           = getOption("suwo_raw_data", FALSE),
    gbif_limit         = 1000L
) {

  # ---- input checks ----------------------------------------------------------

  rank <- tolower(rank)
  valid_ranks <- c("class", "order", "family", "subfamily", "tribe", "genus")
  if (!rank %in% valid_ranks) {
    stop("'rank' must be one of: ", paste(valid_ranks, collapse = ", "))
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    stop(
      "An API key is required for Xeno-Canto API v3.\n",
      "Get yours at https://xeno-canto.org/account.\n",
      "Set it with: Sys.setenv(xc_api_key = 'YOUR_KEY')"
    )
  }

  n_steps <- if (download) 4L else 3L

  # ---- internal helper: safe JSON fetch --------------------------------------

  .fetch_json <- function(url) {
    tryCatch(
      jsonlite::fromJSON(url, simplifyVector = TRUE),
      error = function(e) NULL
    )
  }

  # ---- internal helper: extract accepted species names from a results df -----

  .extract_accepted_spp <- function(df) {
    if (is.null(df$rank) || is.null(df$canonicalName)) return(character(0))
    ok <- !is.na(df$rank) & df$rank == "SPECIES" &
      !is.na(df$taxonomicStatus) & df$taxonomicStatus == "ACCEPTED"
    df$canonicalName[ok]
  }

  # ---- internal helper: recursively page through children of a GBIF key ------

  .get_species_from_key <- function(key) {
    spp <- character(0)
    off <- 0L
    end <- FALSE

    while (!end) {
      url <- paste0(
        "https://api.gbif.org/v1/species/", key, "/children",
        "?limit=", gbif_limit, "&offset=", off
      )
      res <- .fetch_json(url)
      if (is.null(res) || length(res$results) == 0) break

      df <- res$results

      # Harvest any species-rank rows directly
      spp <- unique(c(spp, .extract_accepted_spp(df)))

      # Recurse into non-species children (genera, subfamilies, etc.)
      if (!is.null(df$rank)) {
        sub_keys <- df$key[!is.na(df$rank) & df$rank != "SPECIES" & !is.na(df$key)]
        for (ck in sub_keys) {
          spp <- unique(c(spp, .get_species_from_key(ck)))
        }
      }

      end <- isTRUE(res$endOfRecords)
      off <- off + gbif_limit
    }

    spp
  }

  # ===========================================================================
  # STEP 1 - Resolve GBIF taxon key
  # ===========================================================================

  if (verbose) message(sprintf(
    "[1/%d] Resolving '%s' (%s) in GBIF backbone ...", n_steps, taxon, rank
  ))

  match_url <- paste0(
    "https://api.gbif.org/v1/species/match?name=",
    utils::URLencode(taxon, reserved = TRUE),
    "&rank=", toupper(rank),
    "&strict=false&verbose=false"
  )

  match_result <- .fetch_json(match_url)

  if (is.null(match_result) || is.null(match_result$usageKey)) {
    stop(sprintf(
      "Could not resolve '%s' at rank '%s' in the GBIF backbone.", taxon, rank
    ))
  }

  taxon_key  <- match_result$usageKey
  taxon_name <- match_result$canonicalName %||% taxon

  if (verbose) message(sprintf(
    "    -> Matched: %s (GBIF key: %s)", taxon_name, taxon_key
  ))

  # ===========================================================================
  # STEP 2 - Retrieve species list from GBIF
  # ===========================================================================

  if (verbose) message(sprintf(
    "[2/%d] Fetching species list from GBIF ...", n_steps
  ))

  species_list <- .get_species_from_key(taxon_key)

  # Optional country filter via GBIF occurrence records
  if (!is.null(country) && length(species_list) > 0) {
    if (verbose) message(sprintf(
      "    Filtering %d species by country '%s' via GBIF occurrences ...",
      length(species_list), country
    ))

    has_occurrence <- vapply(species_list, function(sp) {
      occ_url <- paste0(
        "https://api.gbif.org/v1/occurrence/search",
        "?scientificName=", utils::URLencode(sp, reserved = TRUE),
        "&country=", toupper(country),
        "&limit=1"
      )
      res <- .fetch_json(occ_url)
      !is.null(res) && isTRUE(res$count > 0)
    }, logical(1))

    species_list <- species_list[has_occurrence]
  }

  if (length(species_list) == 0) {
    if (verbose) message(
      "    No species found for the given taxon / country combination."
    )
    return(invisible(NULL))
  }

  if (verbose) message(sprintf(
    "    -> %d species to query on Xeno-Canto.", length(species_list)
  ))

  # ===========================================================================
  # STEP 3 - Query Xeno-Canto metadata per species
  # ===========================================================================

  if (verbose) message(sprintf(
    "[3/%d] Querying Xeno-Canto metadata ...", n_steps
  ))

  results_list <- vector("list", length(species_list))

  if (pb && verbose) {
    pb_meta <- utils::txtProgressBar(
      min = 0, max = length(species_list), style = 3
    )
  }

  for (i in seq_along(species_list)) {

    sp <- species_list[[i]]

    xc_query <- paste0('sp:"', sp, '"')
    if (!is.null(xc_query_extra) && nzchar(xc_query_extra)) {
      xc_query <- paste(xc_query, xc_query_extra)
    }

    sp_result <- tryCatch(
      query_xenocanto(
        species  = xc_query,
        cores    = cores,
        pb       = FALSE,
        verbose  = FALSE,
        all_data = all_data,
        raw_data = raw_data,
        api_key  = api_key
      ),
      error = function(e) NULL
    )

    if (!is.null(sp_result) && nrow(sp_result) > 0) {
      sp_result$queried_taxon <- taxon_name
      results_list[[i]] <- sp_result
    }

    if (pb && verbose) utils::setTxtProgressBar(pb_meta, i)
    Sys.sleep(query_delay)
  }

  if (pb && verbose) close(pb_meta)

  results_df <- do.call(
    dplyr::bind_rows,
    Filter(Negate(is.null), results_list)
  )

  if (is.null(results_df) || nrow(results_df) == 0) {
    if (verbose) message(
      "    No Xeno-Canto recordings found for any species in this taxon."
    )
    return(invisible(NULL))
  }

  if (verbose) message(sprintf(
    "    -> %d recording(s) found across %d species.",
    nrow(results_df),
    length(unique(results_df$species))
  ))

  # ===========================================================================
  # STEP 4 (optional) - Download audio files
  # ===========================================================================

  if (!download) return(results_df)

  if (verbose) message(sprintf("[4/%d] Downloading audio files ...", n_steps))

  # Apply quality filter to select which rows to download
  dl_idx <- if (!is.null(quality_filter)) {
    which(results_df$quality %in% quality_filter)
  } else {
    seq_len(nrow(results_df))
  }

  if (length(dl_idx) == 0) {
    if (verbose) message(
      "    No recordings match the quality filter. Nothing downloaded."
    )
    results_df$local_path <- NA_character_
    return(results_df)
  }

  if (verbose && !is.null(quality_filter)) message(sprintf(
    "    %d of %d recording(s) pass quality filter (%s).",
    length(dl_idx), nrow(results_df),
    paste(quality_filter, collapse = "/")
  ))

  dl_df <- results_df[dl_idx, ]

  # Resolve file extensions
  ext <- if ("file_extension" %in% names(dl_df)) {
    dl_df$file_extension
  } else {
    tools::file_ext(dl_df$file_url)
  }
  ext[!nzchar(ext)] <- "mp3"

  # Build safe filenames: Genus_species_XCid.ext
  safe_sp    <- gsub(" ", "_", dl_df$species)
  file_names <- paste0(safe_sp, "_XC", dl_df$key, ".", ext)

  # Resolve destination directories
  dest_dirs <- if (subdir_by_species) {
    file.path(normalizePath(download_path, mustWork = FALSE), safe_sp)
  } else {
    rep(normalizePath(download_path, mustWork = FALSE), nrow(dl_df))
  }

  # Create all needed directories up front
  for (d in unique(dest_dirs)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

  dest_paths <- file.path(dest_dirs, file_names)

  # Initialise local_path column on the full results data frame
  results_df$local_path <- NA_character_

  n_downloaded <- 0L
  n_skipped    <- 0L
  n_failed     <- 0L

  if (pb && verbose) {
    pb_dl <- utils::txtProgressBar(min = 0, max = nrow(dl_df), style = 3)
  }

  for (i in seq_len(nrow(dl_df))) {

    dest <- dest_paths[[i]]

    # Skip if file exists and overwrite is FALSE
    if (!overwrite && file.exists(dest)) {
      n_skipped <- n_skipped + 1L
      results_df$local_path[dl_idx[[i]]] <- dest
      if (pb && verbose) utils::setTxtProgressBar(pb_dl, i)
      next
    }

    ok <- tryCatch({
      utils::download.file(
        url      = dl_df$file_url[[i]],
        destfile = dest,
        mode     = "wb",   # binary - essential for audio
        quiet    = TRUE
      )
      TRUE
    }, error = function(e) {
      if (verbose) message(sprintf(
        "\n    [WARN] Failed XC%s (%s): %s",
        dl_df$key[[i]], dl_df$species[[i]], conditionMessage(e)
      ))
      FALSE
    })

    if (ok) {
      n_downloaded <- n_downloaded + 1L
      results_df$local_path[dl_idx[[i]]] <- dest
    } else {
      n_failed <- n_failed + 1L
    }

    if (pb && verbose) utils::setTxtProgressBar(pb_dl, i)
    Sys.sleep(download_delay)
  }

  if (pb && verbose) close(pb_dl)

  if (verbose) message(sprintf(
    "    Done: %d downloaded | %d skipped (exist) | %d failed.",
    n_downloaded, n_skipped, n_failed
  ))

  return(results_df)
}

# Null-coalescing operator (base R >= 4.4 has it natively; define for compat)
`%||%` <- function(x, y) if (!is.null(x)) x else y
