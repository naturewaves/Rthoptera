% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_song_stats.R
\name{batch_song_stats}
\alias{batch_song_stats}
\title{Apply song_stats functions to Wave Objects in Parallel}
\usage{
batch_song_stats(wave_objects, cores = NULL, q, ...)
}
\arguments{
\item{wave_objects}{A list of \code{Wave} objects or a character vector of object names.}

\item{cores}{Number of CPU cores to use (default: \code{parallel::detectCores() - 1}).}

\item{q}{Character. Either "low" to use the \code{song_stats_lq()} function
or "high" to call the \code{song_stats_hq()} function.}

\item{...}{Additional arguments passed to \code{song_stats_lq()} or \code{song_stats_hq()}.}
}
\value{
A list of merged tibbles (one per output table).
}
\description{
Processes multiple \code{Wave} objects in parallel using \code{song_stats_lq()}, suppresses plots,
and combines results into a structured list of tables.
}
\examples{
\dontrun{
library(tuneR)
wave1 <- sine(440)
wave2 <- noise()
results <- parallel_song_stats(c("wave1", "wave2"), cores = 2)
}

}
