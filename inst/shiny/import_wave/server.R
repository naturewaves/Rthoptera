server = function(input, output, session) {
  # Increase maximum file size to 100 MB
  options(shiny.maxRequestSize = (1000 * 1024 ^ 2))

  # Reactive values to store wave object and selected channel
  waveObject <- shiny::reactiveVal(NULL)
  selectedChannel <- shiny::reactiveVal(NULL)
  audioFilePath <- shiny::reactiveVal(NULL)

  # Function to read and process audio file
  read_and_process_audio <- function(filepath, selected_channel = NULL) {

    # wave <- bioacoustics::read_audio(filepath)
    wave <- tuneR::readWave(filepath)
    if (is.null(selected_channel) || !wave@stereo) {
      selected_channel <- 'left' # Default to the first channel for mono or unspecified channel
    }
    wave <- tuneR::channel(wave, which = selected_channel)
    wave <- seewave::rmoffset(wave, output = "Wave")
    wave
  }

  # Observe file input and read the audio file
  shiny::observeEvent(input$audioFile, {
    shiny::req(input$audioFile)
    audioFilePath(input$audioFile$datapath)
    tryCatch({
      wave <- bioacoustics::read_audio(input$audioFile$datapath)
      if (wave@stereo) {
        shiny::showModal(shiny::modalDialog(
          title = "Stereo Audio Detected",
          shiny::radioButtons("channelSelect", "Select Channel",
                              choices = list("Left (1)" = 'left', "Right (2)" = 'right'),
                              selected = 'left'),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("confirmChannel", "Confirm")
          )
        ))
      } else {
        selectedChannel(1)  # Default to the first channel if mono
        waveObject(read_and_process_audio(input$audioFile$datapath))
      }
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        "Failed to read the audio file. Please try again.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
    })
  })

  # Observe the channel selection and store the selected channel
  shiny::observeEvent(input$confirmChannel, {
    selectedChannel(input$channelSelect)
    waveObject(read_and_process_audio(audioFilePath(), selectedChannel()))
    shiny::removeModal()
  })

  # Observe save button, process the audio with the selected channel, and save the wave object
  shiny::observeEvent(input$saveEditedWave, {
    shiny::req(audioFilePath(), selectedChannel(), input$newName)
    tryCatch({
      wave <- read_and_process_audio(audioFilePath(), selectedChannel())
      waveObject(wave)
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
