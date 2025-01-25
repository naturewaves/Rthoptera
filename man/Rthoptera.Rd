\name{Rthoptera}
\alias{Rthoptera}
\title{Introduction to Rthoptera}

\description{
The \bold{Rthoptera} package provides interactive Shiny applications
for standard analysis and plotting of insect sounds. The package is intended to
be used on high signal-to-noise (focalized) recordings, helping researchers make
standardized measurements and plots to support the scientific description of the
"acoustic types" and subsequent ethological and taxonomic studies. We define
"acoustic type" as the first description of the calling song of a species,
which should be accompanied by a high-quality recording and ideally the voucher
specimen identifier in the collection it was deposited. The package includes
functions for pre-processing, spectral and temporal analysis, as well as several
interactive and static visualizations.

\bold{1. PREPROCESSING}

We developed interactive versions of several common preprocessing tools
borrowed from other packages (e.g., seewave, tuneR, etc.), allowing the user to
monitor the editing process in real time before commiting the edits to new Wave objects.
We encourage new users to see the "Preprocessing" vignette for further guidance.

\bold{Import}: Browse your local data to import audio files (WAV, WAC, or MP3) as
Wave objects into R using the \verb{import_audio} function from the \verb{bioacoustics}
package in the background. During this process, the waveform is automatically
centered to zero with the \verb{rmoffset()} function from \verb{seewave} to
ensure accurate measurements.

\bold{Downsample}: When required, you can reduce the sampling rate of your audio
files. This helps improve the computing speed for certain analyses and plots
without losing important sound features. To help decide if downsampling is appropriate,
an interactive mean power spectrum is shown.

\bold{Band-pass Filter}: When necessary, apply a user-defined high-pass and/or
low-pass filter to eliminate noise or non-target sounds from your audio
recordings. This process can be monitored in the interactive mean power spectrum.

\bold{Trim}: Using an interactive oscillogram, you can visually select and trim
specific sections of a Wave object. The trimmed sections can be saved as new Wave
objects in the R environment for further analysis.



\bold{2. ANALYSIS}

Once pre-processing is complete, you can use different analysis tools to extract
spectral and temporal statistics while interactively monitoringthe results to find
the best set of parameters for your recordings. While we kept separate apps and
functions for different analyses, the \bold{song_statistics} function summarizes
temporal and spectral data simultaneously, expediting the analysis.

\bold{Spectral Statistics}: Automatically calculate spectral metrics based on the
mean power spectrum of a Wave object. These statistics provide insights into the
overall frequency domain of the audio signal.

\bold{Temporal Statistics}: Automatically extract temporal metrics from your audio
recordings. This includes identifying and analyzing elements ("tooth impacts"),
trains (syllables), and echemes (groups of syllables or trills) in the insect sounds.
Two apps are available: \bold{temporal_stats_hq_app} is optimized to work with "tonal"
(i.e., "high-Q") signals, such as those produced by most crickets. It creates an
envelope of the waveform and measures the duration of sounds and gaps based on
a user-defined detection threshold. The \bold{temporal_stats_lq_app} works better
for broadband calls with wide amplitude variability, where the threshold approach
would leave fainter sounds undetected or poorly measured. This app detects each
peak in the envelope, often corresponding to a single tooth impact, and groups
them into trains and echemes with user-defined thresholds.

\bold{Song Statistics}: A newer version to automatically extract temporal and
spectral metrics for each detection within a recording. It combines the two
functions described above and is probably the only one you might want to use.

\bold{3. PLOTTING}

\bold{Multi-Power Spectrum}: An interactive tool that overlays multiple power
spectrum plots selected from the oscillogram, allowing for easy comparison and
visualization of spectral features across different time intervals. Each selection
is assigned to its own color-blind-safe color both in the oscillogram and the mean
spectrum plots.

\bold{Spectrogram}: Generate standard spectrograms, optionally alongside a
lateral mean power spectrum. This combination allows you to visualize both
time-frequency representations and the overall spectral distribution.
The spectrogram window size is automatically adjusted based on the sampling
rate and duration of the recording to obtain a standard frequency/time resolution
trade-off.

\bold{Multi Plot}: A combined visualization of the spectrogram, mean spectrum,
and oscillogram, providing a comprehensive view of both the time-domain and
frequency-domain characteristics of the insect sounds.

\bold{Oscillogram}: Create standard oscillograms as well as interactive
oscillograms that allow you to zoom in and explore specific sections of the
waveform.

\bold{Multi-oscillogram}: Create a stacked oscillogram plot for comparing the
sounds of multiple species. This is particularly useful for analyzing the
differences in acoustic patterns between species.


\bold{INSTALLATION}

To install the Rthoptera package, follow these steps:

Install the remotes package if you haven't already:

\verb{install.packages("remotes")}


Load the remotes package:

\verb{library(remotes)}

Install Rthoptera from GitHub:

\verb{remotes::install_github("naturewaves/Rthoptera")}

Load the Rthoptera package:

\verb{library(Rthoptera)}
}
