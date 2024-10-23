server = function(input, output, session) {
  # Function to create the multi-oscillogram plot
  multiOscillogram <- function(waves, maxDur, scalebarLength, allScalebar = FALSE) {
    max_samples <- maxDur * waves[[1]]@samp.rate
    wave_labels <- LETTERS[1:length(waves)]

    all_waves_df <- lapply(seq_along(waves), function(i) {
      wave <- waves[[i]]
      samp_rate <- wave@samp.rate
      wave_length <- length(wave@left)
      duration <- wave_length / samp_rate

      time <- seq(-maxDur / 2, maxDur / 2, length.out = max_samples)
      amplitude <- rep(0, max_samples)

      start_index <- round((max_samples - wave_length) / 2)
      end_index <- start_index + wave_length - 1

      amplitude[start_index:end_index] <- wave@left

      # Subtract mean and rescale to [-1, 1]
      amplitude <- amplitude - mean(amplitude)
      amplitude <- 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1

      data.frame(
        time = time,
        amplitude = amplitude,
        wave_name = wave_labels[i],
        id = i
      )
    }) |> dplyr::bind_rows()

    plots <- lapply(unique(all_waves_df$id), function(id) {
      p <- ggplot2::ggplot(all_waves_df %>% dplyr::filter(id == !!id), ggplot2::aes(x = time, y = amplitude)) +
        ggplot2::geom_line(color = "black", linewidth = 0.5) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::coord_cartesian(xlim = c(-maxDur / 2, maxDur / 2)) +
        ggplot2::expand_limits(y = c(-1.2, 1.2))

      # Add scalebar to each oscillogram
      if (allScalebar) {
        if (scalebarLength < 1) {
          scalebarText <- paste0(scalebarLength * 1000, " ms")
        } else {
          scalebarText <- paste0(scalebarLength, " s")
        }
        p <- p +
          ggplot2::annotate("segment", x = -maxDur / 2 + 0.05, xend = -maxDur / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05, color = "black", size = 1) +
          ggplot2::annotate("text", x = -maxDur / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
      } else {
        if (id == length(waves)) {  # Add scale bar only to the last plot
          if (scalebarLength < 1) {
            scalebarText <- paste0(scalebarLength * 1000, " ms")
          } else {
            scalebarText <- paste0(scalebarLength, " s")
          }
          p <- p +
            ggplot2::annotate("segment", x = -maxDur / 2 + 0.05, xend = -maxDur / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05, color = "black", size = 1) +
            ggplot2::annotate("text", x = -maxDur / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
        }
      }

      return(p)
    })

    combined_plot <- patchwork::wrap_plots(plots, ncol = 1)

    return(combined_plot)
  }

  # Store reactive values
  values <- shiny::reactiveValues(
    colorSelections = list(),
    history = list(),
    plotFile = NULL # Store the temporary file path
  )

  # Observer to update available wave objects in the environment
  shiny::observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
      inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObjects", choices = waveObjects)
  })

  # This reactive expression will re-run only when the "Plot Multi-Oscillogram" button is clicked
  result <- shiny::eventReactive(input$plotMultiOscillogram, {
    shiny::req(input$waveObjects)
    waves <- lapply(input$waveObjects, function(x) get(x, envir = .GlobalEnv))
    combined_plot <- multiOscillogram(waves, input$maxDur, input$scalebarLength, input$allScalebar)

    # Calculate image dimensions
    img_width <- input$imgWidth
    img_height <- input$imgHeight * length(waves) # Multiply height by number of oscillograms

    # Save the plot to a temporary file
    temp_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(temp_file, plot = combined_plot, width = img_width, height = img_height, units = "in", dpi = 300)
    values$plotFile <- temp_file

    combined_plot
  })

  output$plotOutput <- shiny::renderUI({
    shiny::req(result())
    shiny::req(input$plotMultiOscillogram)
    shinycssloaders::withSpinner(shiny::plotOutput("multiWavePlot",
                                                   height = paste0(100 * length(input$waveObjects), "px"),
                                                   width = "1450px"))
  })

  output$multiWavePlot <- shiny::renderPlot({
    shiny::req(result())
    result()
  })

  shiny::observeEvent(input$waveObjects, {
    valid_waveObjects <- input$waveObjects[sapply(input$waveObjects, function(wave_name) {
      wave <- get(wave_name, envir = .GlobalEnv)
      wave_dur <- seewave::duration(wave)
      wave_dur <= input$maxDur
    })]

    if (length(valid_waveObjects) < length(input$waveObjects)) {
      shiny::showModal(shiny::modalDialog(
        title = "Warning",
        "All waves should be equal or shorter than Max. Duration."
      ))
      shiny::updateSelectInput(session, "waveObjects", selected = valid_waveObjects)
    }
  })

  # Download handler for saving the image
  output$saveImage <- shiny::downloadHandler(
    filename = function() {
      paste("_multi_oscillo.png")
    },
    content = function(file) {
      shiny::req(values$plotFile)
      file.copy(values$plotFile, file, overwrite = TRUE)
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
