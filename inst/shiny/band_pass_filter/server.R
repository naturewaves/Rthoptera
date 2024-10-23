server = function(input, output, session) {

  waveObject <- reactiveVal(NULL)
  # plotly_obj <- reactiveVal()

  # Function to update wave object choices
  update_wave_choices <- function() {
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    updateSelectInput(session, "selectedWave", choices = waveObjects)
  }

  # Observe to update wave object choices initially and on refresh
  observe({
    update_wave_choices()
  })

  # observeEvent(input$refresh, {
  #   update_wave_choices()
  # })

  # Update the reactive waveObject whenever the selection changes
  observeEvent(input$selectedWave, {
    req(input$selectedWave)
    tryCatch({
      newWave <- get(input$selectedWave, envir = .GlobalEnv)
      waveObject(newWave)

      # Update the lowpass value to Nyquist frequency (wave@samp.rate / 2)
      updateNumericInput(session, "lowpass", value = newWave@samp.rate / 2000)  # Divide by 2000 to convert Hz to kHz

    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to load the selected wave object. Please try again.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })

  output$audioPlot <- renderPlotly({
    req(waveObject())
    req(input$plotMeanSpectrum)
    meanspectrum_plotly(waveObject())
  })


  observeEvent(input$plotMeanSpectrum, {
    req(waveObject())
    output$audioPlot <- renderPlotly({
      meanspectrum_plotly(waveObject())
    })
  })

  output$audioPlot <- renderPlotly({
    req(waveObject())
    req(input$plotSpectro)
    spectrogram_plotly(waveObject())
  })


  observeEvent(input$plotSpectro, {
    req(waveObject())
    output$audioPlot <- renderPlotly({
      spectrogram_plotly(waveObject())
    })
  })

  observeEvent(input$applyFilter, {
    req(waveObject())
    tryCatch({
      filtered_wave <- seewave::fir(
        waveObject(),
        from = isolate(input$highpass) * 1000,
        to = isolate(input$lowpass) * 1000,
        bandpass = TRUE,
        output = "Wave"
      )
      waveObject(filtered_wave)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to apply the band-pass filter. Please try again.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })

  observeEvent(input$saveEditedWave, {
    req(waveObject(), input$newName)
    tryCatch({
      assign(input$newName, waveObject(), envir = .GlobalEnv)
      showModal(modalDialog(
        title = "Saved!",
        paste0("Available as '", input$newName, "' in the R environment."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to save the wave object. Please try again.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })



  # Stop app when the tab is closed with the "X" button
  session$onSessionEnded(function() {
    stopApp()
  })

  # Stop app when the "Close app" button is used
  observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    stopApp()
  })



}
