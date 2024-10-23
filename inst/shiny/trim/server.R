server <- function(input, output, session) {

  waveObject <- shiny::reactiveVal(NULL)
  plotVisible <- shiny::reactiveVal(TRUE)
  selectedRegion <- shiny::reactiveVal(NULL)
  zoomedRegion <- shiny::reactiveVal(NULL)

  oscillo3 <- function(wave) {
    oscillo_data <- seewave::oscillo(wave, plot = FALSE)
    time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
    amplitude <- oscillo_data / max(abs(oscillo_data))
    oscillo_df <- data.frame(time = time, amplitude = amplitude)

    ggplot2::ggplot(oscillo_df, ggplot2::aes(x = time, y = amplitude)) +
      ggplot2::geom_line(color = "white") +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(n.breaks = 3, expand = c(0.1, 0.1)) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 15, r = 10, b = 15, l = 10, unit = 'pt'),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "#274C77", color = "#274C77"),
        plot.background = ggplot2::element_rect(fill = "#274C77", color = "#274C77"),
        axis.line.y = ggplot2::element_line(colour = "white"),
        axis.line.x = ggplot2::element_line(colour = "white"),
        axis.ticks.x = ggplot2::element_line(colour = "white"),
        axis.ticks.y = ggplot2::element_line(colour = "white"),
        axis.title.x = ggplot2::element_text(colour = "white"),
        axis.text.x = ggplot2::element_text(colour = "white"),
        axis.title = ggplot2::element_text(size = 10, colour = "white"),
        axis.text = ggplot2::element_text(size = 10, colour = "white"),
        legend.position = "none"
      ) +
      ggplot2::labs(y = "Relative Amplitude", x = "Time (s)")
  }

  update_wave_choices <- function() {
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "selectedWave", choices = waveObjects)
  }

  shiny::observe({
    update_wave_choices()
  })

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

  output$wavePlot <- shiny::renderPlot({
    shiny::req(input$plot)
    shiny::req(plotVisible())
    wave <- get(input$selectedWave, envir = .GlobalEnv)
    if (!is.null(wave)) {
      if (!is.null(zoomedRegion())) {
        extract <- tuneR::extractWave(wave, from = zoomedRegion()[1], to = zoomedRegion()[2], xunit = "time")
        oscillo3(extract)
      } else {
        oscillo3(wave)
      }
    }
  }, height = 400, width = 1500)

  shiny::observeEvent(input$waveBrush, {
    currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
    if (!is.null(zoomedRegion())) {
      currentRegion <- c(zoomedRegion()[1] + currentRegion[1], zoomedRegion()[1] + currentRegion[2])
    }
    selectedRegion(currentRegion)
  })

  shiny::observeEvent(input$zoomIn, {
    shiny::req(selectedRegion())
    zoomedRegion(selectedRegion())
    selectedRegion(NULL)
  })

  shiny::observeEvent(input$zoomOut, {
    if (!is.null(zoomedRegion())) {
      zoomRange <- zoomedRegion()[2] - zoomedRegion()[1]
      newStart <- max(0, zoomedRegion()[1] - zoomRange * 0.1)
      newEnd <- min(get(input$selectedWave, envir = .GlobalEnv)@samp.rate, zoomedRegion()[2] + zoomRange * 0.1)
      if (newEnd - newStart >= get(input$selectedWave, envir = .GlobalEnv)@samp.rate) {
        zoomedRegion(NULL)
      } else {
        zoomedRegion(c(newStart, newEnd))
      }
    }
  })

  shiny::observeEvent(input$saveSelection, {
    shiny::req(selectedRegion(), input$selectedWave, input$selectionName)
    wave <- get(input$selectedWave, envir = .GlobalEnv)
    selection <- tuneR::extractWave(wave, from = selectedRegion()[1], to = selectedRegion()[2], xunit = "time")
    assign(input$selectionName, selection, envir = .GlobalEnv)
    shiny::showModal(shiny::modalDialog(
      title = "Saved",
      paste0("Available as '", input$selectionName, "' in the R environment."),
      easyClose = TRUE,
      footer = shiny::modalButton("OK")
    ))
  })

  shiny::observeEvent(input$plot, {
    plotVisible(TRUE)
    selectedRegion(NULL)
    zoomedRegion(NULL)
  })

  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

}
