server = function(input, output, session) {
  waveObject <- shiny::reactiveVal(NULL)
  plotly_obj <- shiny::reactiveVal()

  meanspectrum_plotly <- function(wave,
                                  background = '#274C77',
                                  foreground = "white",
                                  hover_bgcolor = "white",
                                  hover_fontcolor = "black") {

    mean_spectrum <- seewave::meanspec(wave,
                                       f = wave@samp.rate,
                                       wl = 1024,
                                       ovlp = 50,
                                       plot = FALSE)
    mean_spectrum_df <- data.frame(
      freq = mean_spectrum[, 1],
      mean_amp = mean_spectrum[, 2]
    )

    plotly::plot_ly(data = mean_spectrum_df, x = ~freq, y = ~mean_amp, type = 'scatter', mode = 'lines', line = list(color = 'white')) %>%
      plotly::add_ribbons(ymin = 0, ymax = ~mean_amp, fillcolor = foreground, line = list(color = foreground)) %>%
      plotly::layout(
        title = "",
        xaxis = list(
          title = "Frequency (kHz)",
          titlefont = list(size = 10, color = foreground),
          tickfont = list(size = 10, color = foreground),
          ticks = "outside",
          tickcolor = foreground,
          tickwidth = 1,
          linecolor = foreground,
          ticklen = 5,
          automargin = TRUE,
          zeroline = FALSE,
          showline = TRUE
        ),
        yaxis = list(
          title = "Mean Amplitude",
          titlefont = list(size = 10, color = foreground),
          tickfont = list(size = 10, color = foreground),
          ticks = "outside",
          tickvals = pretty(mean_spectrum_df$mean_amp, n = 3),
          tickcolor = foreground,
          tickwidth = 1,
          ticklen = 5,
          rangemode= 'tozero',
          linecolor = foreground,
          zeroline = FALSE,
          showline = TRUE
        ),
        paper_bgcolor = background,
        plot_bgcolor = background,
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = max(mean_spectrum_df$freq),
            xref = "x",
            y0 = 0.1,
            y1 = 0.1,
            yref = "y",
            line = list(
              color = foreground,
              dash = "dot"
            )
          )
        ),
        margin = list(
          l = 50,
          r = 10,
          b = 60,
          t = 50
        ),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = TRUE) %>%
      plotly::style(
        hovertemplate = paste0(
          "Frequency: %{x:.1f} kHz<br>",
          "<extra></extra>"
        )
      )
  }

  # Function to update wave object choices
  update_wave_choices <- function() {
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "selectedWave", choices = waveObjects)
  }

  # Observe to update wave object choices initially and on refresh
  shiny::observe({
    update_wave_choices()
  })

  # Update the reactive waveObject whenever the selection changes
  shiny::observeEvent(input$selectedWave, {
    shiny::req(input$selectedWave)
    tryCatch({
      newWave <- get(input$selectedWave, envir = .GlobalEnv)
      waveObject(newWave)
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        "Failed to load the selected wave object. Please try again.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  output$audioPlot <- plotly::renderPlotly({
    shiny::req(waveObject())
    shiny::req(input$plotMeanSpectrum)
    p <- meanspectrum_plotly(waveObject())

    plotly_obj(p)
    p
  })

  shiny::observeEvent(input$downsample, {
    shiny::req(waveObject())
    tryCatch({
      resampled_wave <- seewave::resamp(waveObject(),
                                        g = (as.numeric(input$maxfreq) * 1000) * 2, # from kHz to Hz, from Nyquist to sampling rate
                                        output = "Wave"
      )
      waveObject(resampled_wave)
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        "Failed to downsample the wave object. Please try again.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  shiny::observeEvent(input$saveEditedWave, {
    shiny::req(waveObject(), input$newName)
    tryCatch({
      assign(input$newName, waveObject(), envir = .GlobalEnv)
      shiny::showModal(shiny::modalDialog(
        title = "Saved!",
        paste0("Available as '", input$newName, "' in the R environment."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        "Failed to save the wave object. Please try again.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  # Define the help modal dialog
  shiny::observeEvent(input$help, {
    shiny::showModal(shiny::modalDialog(
      title = "Help",
      shiny::HTML("
          Researchers often use ultrasonic equipment during a recording session
          targeting a broad range of taxa. The resulting collection often includes
          recordings of crickets singing around 5 kHz sampled at 192 kHz or higher.
          In this scenario, downsampling might help improve the speed of plot
          rendering and measurements.<br><br>
          The rule of thumb recordists use to avoid artifacts such as aliasing
          is to select a sampling rate that is higher than double the maximum signal
          of interest. For example, suppose the maximum frequency in the song
          of a cricket is 5 kHz. The minimum sampling rate required to capture
          the signal would be somewhere higher than 10 kHz. However, to maintain
          consistency in the resolution of spectral and temporal analyses across
          recordings, we fixed the lower sampling rate limit to 48 kHz.<br><br>
          TIP: Use the cursor to hover over the Mean Power Spectrum and identify
          the maximum frequency of interest in the recording.
        "),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  })

  # Stop app when the tab is closed with the "X" button
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  # Stop app when the "Close app" button is used
  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })
}
