server <- function(input, output, session) {

  selectedRegion <- shiny::reactiveVal(NULL)
  finalZoomedRegion <- shiny::reactiveVal(NULL)
  thirdZoomedRegion <- shiny::reactiveVal(NULL)

  fullPlot <- shiny::reactiveVal(NULL)
  zoomedPlot <- shiny::reactiveVal(NULL)
  finalZoomedPlot <- shiny::reactiveVal(NULL)

  tempImagePath <- shiny::reactiveVal(NULL)  # Path to the temporary image file

  plotTriggered <- shiny::reactiveVal(FALSE)

  wave_df <- function(wave, norm = TRUE) {
    if (norm) {
      wave <- tuneR::normalize(object = wave, unit = "1", center = TRUE)
    }

    wave <- seewave::rmoffset(wave, output = "Wave")
    srate <- wave@samp.rate
    amplitude <- wave@left

    tbl <- tibble::tibble(amplitude = amplitude)
    tbl <- dplyr::mutate(tbl, index = dplyr::row_number(), time = (index - 1) / srate)
    tbl <- dplyr::select(tbl, c(amplitude, time))

    return(tbl)
  }

  createOscillogram <- function(wave, brush_data = list(), colors = NULL, display_option = "axes", scalebar_length = NULL, vertical_lines = NULL) {

    tbl <- wave_df(wave)

    p <- ggplot2::ggplot(tbl, ggplot2::aes(x = time, y = amplitude)) +
      ggplot2::geom_line(color = "black", linewidth = 0.4) +
      ggplot2::expand_limits(y = c(-1.2, 1.2)) +
      ggplot2::labs(x = "Time (s)", y = "Amplitude") +
      ggplot2::scale_x_continuous(expand = c(0, 0))

    if (display_option == "axes") {
      p <- p + ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"),
          axis.title = ggplot2::element_text(size = 14)
        )
    } else if (display_option == "scalebar" && !is.null(scalebar_length)) {
      scalebar_length_sec <- scalebar_length / 1000  # Convert ms to seconds
      scalebarText <- ifelse(scalebar_length < 1000, paste0(scalebar_length, " ms"), paste0(scalebar_length_sec, " s"))

      scalebar_x_start <- 0.5 / input$imgWidth * max(tbl$time)
      scalebar_x_end <- scalebar_x_start + scalebar_length_sec

      if (scalebar_length_sec > max(tbl$time)) {
        shiny::showModal(shiny::modalDialog(
          title = "Scale Bar Too Long",
          "The scale bar length exceeds the duration of the oscillogram. Please select a shorter scale bar length.",
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
      } else {
        p <- p + ggplot2::theme_void() +
          ggplot2::annotate("segment", x = scalebar_x_start, xend = scalebar_x_end, y = -1.1, yend = -1.1, color = "black", linewidth = 1) +
          ggplot2::annotate("text", x = (scalebar_x_start + scalebar_x_end) / 2, y = -1.3, label = scalebarText, vjust = 0.5, hjust = 0.5)
      }
    }

    if (!is.null(vertical_lines)) {
      p <- p + ggplot2::geom_vline(xintercept = vertical_lines, col = "grey20", linewidth = 1, linetype = "dashed")
    }

    return(p)
  }

  selected_wave <- shiny::reactiveVal()
  brushed_ranges <- shiny::reactiveVal(list())
  tempWave <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$waveObject, {
    if (input$waveObject != "") {
      wave_obj <- get(input$waveObject, envir = .GlobalEnv)
      selected_wave(wave_obj)
      brushed_ranges(list())
    }
  })

  shiny::observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObject", choices = waveObjects)
  })

  output$wavePlotUI <- shiny::renderUI({
    if (plotTriggered()) {
      shinycssloaders::withSpinner(
        shiny::plotOutput("wavePlot", height = "250px", width = "1500px", brush = shiny::brushOpts(id = "waveBrush", direction = "x")),
        type = 1
      )
    }
  })

  shiny::observeEvent(input$oscillogram, {
    shiny::req(input$waveObject)

    plotTriggered(TRUE)
    selectedRegion(NULL)
    finalZoomedRegion(NULL)
    thirdZoomedRegion(NULL)
    tempWave(NULL)
    fullPlot(NULL)
    zoomedPlot(NULL)
    finalZoomedPlot(NULL)
    tempImagePath(NULL)

    shiny::removeUI(selector = "#zoomedPlot")
    shiny::removeUI(selector = "#finalZoomedPlot")

    wave <- get(input$waveObject, envir = .GlobalEnv)

    shiny::isolate({
      output$wavePlot <- shiny::renderPlot({
        tryCatch({
          if (!is.null(wave)) {
            p <- createOscillogram(wave, brushed_ranges(), display_option = input$displayOption, scalebar_length = input$scaleBar1)
            if (!is.null(finalZoomedRegion()) && length(finalZoomedRegion()) == 2) {
              p <- p + ggplot2::geom_vline(xintercept = finalZoomedRegion(), col = "grey20", linewidth = 1, linetype = "dashed")
            }
            fullPlot(p)
            p
          }
        }, error = function(e) {
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            paste("An error occurred:", e$message),
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          NULL
        })
      }, height = 250, width = 1500)
    })
  })

  shiny::observeEvent(input$waveBrush, {
    currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
    selectedRegion(currentRegion)
  })

  shiny::observeEvent(input$zoomedPortion, {
    shiny::req(selectedRegion())

    wave <- get(input$waveObject, envir = .GlobalEnv)

    shiny::isolate({
      selected_duration <- diff(selectedRegion())

      if (is.null(finalZoomedRegion())) {
        if (input$displayOption == "scalebar" && input$scaleBar2 / 1000 > selected_duration) {
          shiny::showModal(shiny::modalDialog(
            title = "Scale Bar Too Long",
            "The scale bar length exceeds the duration of the selected portion. Please select a shorter scale bar length.",
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          return()
        }

        finalZoomedRegion(selectedRegion())

        temp_wave <- tuneR::extractWave(wave, from = finalZoomedRegion()[1], to = finalZoomedRegion()[2], xunit = "time")

        tempWave(temp_wave)

        output$wavePlotUI <- shiny::renderUI({
          shiny::plotOutput("wavePlot", height = "250px", width = "1500px")
        })

        output$zoomedPlotUI <- shiny::renderUI({
          shiny::plotOutput("zoomedPlot", height = "250px", width = "1500px", brush = shiny::brushOpts(id = "zoomedBrush", direction = "x"))
        })

        output$zoomedPlot <- shiny::renderPlot({
          shiny::req(finalZoomedRegion())

          tryCatch({
            if (!is.null(tempWave())) {
              p <- createOscillogram(tempWave(), display_option = input$displayOption, scalebar_length = input$scaleBar2)
              zoomedPlot(p)
              p
            }
          }, error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Error",
              paste("An error occurred:", e$message),
              easyClose = TRUE,
              footer = shiny::modalButton("OK")
            ))
            NULL
          })
        }, height = 250, width = 1500)

      } else {
        shiny::req(tempWave())

        if (input$displayOption == "scalebar" && input$scaleBar3 / 1000 > selected_duration) {
          shiny::showModal(shiny::modalDialog(
            title = "Scale Bar Too Long",
            "The scale bar length exceeds the duration of the selected portion. Please select a shorter scale bar length.",
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          return()
        }

        third_zoomed_wave <- tuneR::extractWave(tempWave(), from = input$zoomedBrush$xmin, to = input$zoomedBrush$xmax, xunit = "time")

        thirdZoomedRegion(c(input$zoomedBrush$xmin, input$zoomedBrush$xmax))

        output$finalZoomedPlotUI <- shiny::renderUI({
          shiny::plotOutput("finalZoomedPlot", height = "250px", width = "1500px")
        })

        output$finalZoomedPlot <- shiny::renderPlot({
          shiny::req(thirdZoomedRegion())

          tryCatch({
            if (!is.null(third_zoomed_wave)) {
              p <- createOscillogram(third_zoomed_wave, display_option = input$displayOption, scalebar_length = input$scaleBar3)
              finalZoomedPlot(p)

              temp_file <- tempfile(fileext = ".png")
              total_height <- input$imgHeight * 3  # 3 plots (full, zoomed, and final zoomed)
              combinedPlot <- cowplot::plot_grid(fullPlot(), zoomedPlot(), finalZoomedPlot(), ncol = 1)
              bg <- ifelse(input$transparentBg, "transparent", "white")

              ggplot2::ggsave(temp_file, combinedPlot, width = input$imgWidth, height = total_height, units = "in", dpi = 300, bg = bg)
              tempImagePath(temp_file)
              p
            }
          }, error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Error",
              paste("An error occurred:", e$message),
              easyClose = TRUE,
              footer = shiny::modalButton("OK")
            ))
            NULL
          })
        }, height = 250, width = 1500)

        output$zoomedPlot <- shiny::renderPlot({
          shiny::req(finalZoomedRegion())

          tryCatch({
            if (!is.null(tempWave())) {
              p <- createOscillogram(tempWave(), display_option = input$displayOption, scalebar_length = input$scaleBar2, vertical_lines = thirdZoomedRegion())
              zoomedPlot(p)
              p
            }
          }, error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Error",
              paste("An error occurred:", e$message),
              easyClose = TRUE,
              footer = shiny::modalButton("OK")
            ))
            NULL
          })
        }, height = 250, width = 1500)
      }
    })
  })

  output$saveImage <- shiny::downloadHandler(
    filename = function() {
      paste0(as.character(input$waveObject), "_oscillogram.png")
    },
    content = function(file) {
      if (!is.null(tempImagePath())) {
        file.copy(tempImagePath(), file)
      } else {
        shiny::req(fullPlot(), zoomedPlot(), finalZoomedPlot())

        total_height <- input$imgHeight * 3  # 3 plots (full, zoomed, and final zoomed)
        combinedPlot <- cowplot::plot_grid(fullPlot(), zoomedPlot(), finalZoomedPlot(), ncol = 1)

        bg <- ifelse(input$transparentBg, "transparent", "white")
        ggplot2::ggsave(file, combinedPlot, width = input$imgWidth, height = total_height, units = "in", dpi = 300, bg = bg)
      }
    }
  )

  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

}
