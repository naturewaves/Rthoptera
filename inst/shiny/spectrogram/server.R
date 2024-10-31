server <- function(input, output, session) {
  plotVisible <- shiny::reactiveVal(FALSE)
  savedPlot <- shiny::reactiveVal(NULL)
  savedImage <- shiny::reactiveVal(NULL)

  shiny::observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
      inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObject", choices = waveObjects)
  })

  shiny::observeEvent(input$plotSpectro, {
    shiny::req(input$waveObject)
    wave <- get(input$waveObject, envir = .GlobalEnv)

    plotVisible(TRUE)

    output$specPlotOutput <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput("specPlot", height = "auto", width = "auto"), type = 1)
    })

    output$specPlot <- shiny::renderPlot({
      tryCatch({
        if (shiny::isolate(input$meanspec)) {
          shiny::req(input$meanspecScale)  # Ensure meanspecScale is available
          scale_type <- shiny::isolate(as.character(input$meanspecScale))
        } else {
          scale_type <- NULL
        }
        combined_plot <- spectrogram_ggplot(wave,
                                            meanspec = shiny::isolate(input$meanspec),
                                            cutoff = shiny::isolate(input$noise.floor),
                                            scale_type = scale_type,
                                            overlap = shiny::isolate(input$overlap))
        print(combined_plot)
        savedPlot(combined_plot)  # Save the plot reactively

        # Save the rendered image to a temporary file
        temp_file <- tempfile(fileext = ".png")
        bg <- ifelse(input$transparentBg, "transparent", "white")
        ggplot2::ggsave(temp_file,
                        plot = combined_plot,
                        width = shiny::isolate(input$imgWidth), height = shiny::isolate(input$imgHeight),
                        units = "in", dpi = 300, bg = bg)

        savedImage(temp_file)  # Save the temporary file path reactively

      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        NULL
      })
    }, width = function() { shiny::isolate(input$imgWidth) * 100 },  # convert to pixels using 100 dpi
    height = function() { shiny::isolate(input$imgHeight) * 100 }
    )
  })

  output$saveImage <- shiny::downloadHandler(
    filename = function() {
      paste(as.character(input$waveObject), "_spectrogram.png", sep = "")
    },
    content = function(file) {
      shiny::req(savedImage())
      file.copy(savedImage(), file)
    }
  )

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
