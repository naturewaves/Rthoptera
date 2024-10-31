server <- function(input, output, session) {

  spectrum_plot2 <- function(wave,
                            wl = 4096,
                            ovlp = 75,
                            scale = "linear",
                            y.min = -100, # use only when scale = "dB"
                            y.breaks = c(-40, -20, 0), # use only when scale = "dB"
                            x.breaks = 6,
                            y.position = "left",
                            x.position = "bottom",
                            flip = FALSE,
                            fill = 'black',
                            fun = "mean",
                            wn = "blackman",
                            show.x.title = TRUE,
                            show.y.title = TRUE,
                            add.params = TRUE,
                            plot.title = NULL,
                            italic_title = FALSE,
                            fmin = NULL,
                            fmax = NULL) {

    # Automatically set fmax to Nyquist frequency if not provided
    if (is.null(fmax) || fmax == 0) {
      fmax <- wave@samp.rate / 2 / 1000  # Convert to kHz
    }

    fmin <- as.numeric(fmin)
    fmax <- as.numeric(fmax)

    # Calculate meanspec_data
    if (scale == "dBFS") {
      meanspec_data <- seewave::meanspec(wave,
                                         f = wave@samp.rate,
                                         wl = wl,
                                         ovlp = ovlp,
                                         plot = FALSE,
                                         norm = FALSE,  # Don't normalize for dBFS
                                         wn = wn,
                                         FUN = fun,
                                         flim = c(fmin, fmax))

      amp_max <- if (wave@bit == 16) 32768 else if (wave@bit == 24) 8388607 else if (wave@bit == 32) 2147483647 else stop("Unsupported bit depth")
      meanspec_data[, 2] <- 20 * log10(abs(meanspec_data[, 2]) / amp_max)

    } else {
      meanspec_data <- seewave::meanspec(wave,
                                         f = wave@samp.rate,
                                         wl = wl,
                                         ovlp = ovlp,
                                         plot = FALSE,
                                         norm = TRUE,
                                         dB = if (scale == "dB") "max0" else NULL,
                                         wn = wn,
                                         FUN = fun,
                                         flim = c(fmin, fmax))
    }

    meanspec_data <- data.frame(
      freq = meanspec_data[, 1],
      mean_amp = meanspec_data[, 2]
    )

    plot <- meanspec_data %>%
      ggplot2::ggplot(ggplot2::aes(x = freq, y = mean_amp)) +
      ggplot2::theme_minimal(base_size = 15)

    if (scale == "dB" || scale == "dBFS") {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(x = freq, ymin = y.min, ymax = mean_amp), fill = fill) +
        ggplot2::scale_y_continuous(breaks = y.breaks, limits = c(y.min, 0), expand = ggplot2::expansion(mult = c(0, .1)), position = y.position)
    } else {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(x = freq, ymin = 0, ymax = mean_amp), fill = fill) +
        ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), expand = ggplot2::expansion(mult = c(0, .1)), position = y.position,
                                    labels = function(x) ifelse(x == 0 | x == 1, as.character(as.integer(x)), as.character(x)))
    }

    title_style <- if (italic_title) "italic" else "plain"

    plot <- plot +
      ggplot2::scale_x_continuous(limits = c(fmin, fmax), expand = c(0, 0), position = x.position, breaks = scales::breaks_pretty(n = x.breaks), labels = scales::label_number(zero.print = "")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        axis.ticks.y = ggplot2::element_line(colour = "black"),
        axis.ticks.x = ggplot2::element_line(colour = "black"),
        axis.title = ggplot2::element_text(size = 12),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 10),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "none",
        plot.title = ggplot2::element_text(face = title_style)
      ) +
      ggplot2::labs(
        x = if (show.x.title) "Frequency (kHz)" else NULL,
        y = if (show.y.title) "Amplitude" else NULL,
        title = plot.title
      )

    if (add.params) {
      params_text <- paste0("win. function: ", wn,
                            "\nwin. size: ", wl,
                            "\noverlap:", ovlp, "%",
                            "\nfunction: ", fun,
                            "\nscale: ", scale)

      plot <- plot +
        ggplot2::annotate("text", x = Inf, y = Inf, label = params_text, hjust = 1.1, vjust = 1.1, size = 4, color = "black")
    }

    if (flip) {
      plot <- plot +
        ggplot2::coord_flip()
    }

    return(plot)
  }

  shiny::observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObject", choices = waveObjects)
  })

  shiny::observeEvent(input$waveObject, {
    shiny::req(input$waveObject)
    wave <- get(input$waveObject, envir = .GlobalEnv)

    nyquist_freq <- wave@samp.rate / 2 / 1000  # Convert to kHz

    shiny::updateNumericInput(session, "fmax", value = nyquist_freq)
  })

  plotData <- shiny::eventReactive(input$plot, {
    shiny::req(input$waveObject)
    wave <- get(input$waveObject, envir = .GlobalEnv)

    fmax_value <- if (is.null(input$fmax) || input$fmax == "") {
      wave@samp.rate / 2 / 1000  # Convert to kHz
    } else {
      input$fmax
    }

    yBreaks <- as.numeric(unlist(strsplit(isolate(input$yBreaks), ",")))

    list(
      plot = spectrum_plot2(wave,
                           wl = isolate(as.numeric(input$wl)),
                           plot.title = isolate(input$plotTitle),
                           italic_title = isolate(input$italicTitle),
                           ovlp = isolate(input$ovlp),
                           scale = isolate(input$scale),
                           y.min = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") isolate(input$yMin) else NULL,
                           y.breaks = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") yBreaks else NULL,
                           x.breaks = isolate(input$x.breaks),
                           y.position = isolate(input$yPosition),
                           x.position = isolate(input$xPosition),
                           flip = isolate(input$flip),
                           fill = isolate(input$fill),
                           fun = isolate(input$fun),
                           wn = isolate(input$wn),
                           show.x.title = isolate(input$showXTitle),
                           show.y.title = isolate(input$showYTitle),
                           add.params = isolate(input$addParams),
                           fmin = input$fmin,
                           fmax = fmax_value),
      width = isolate(input$plotWidth),
      height = isolate(input$plotHeight)
    )
  })

  output$plot <- shiny::renderPlot({
    shiny::req(plotData())
    print(plotData()$plot)
  },
  width = 'auto', height = 'auto'
  )

  savedImage <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$plot, {
    shiny::req(plotData())
    temp_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(temp_file, plot = plotData()$plot, width = plotData()$width, height = plotData()$height, dpi = 300, bg = "white")
    savedImage(temp_file)
  })

  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste(input$waveObject, "_spectral_plot.png", sep = "")
    },
    content = function(file) {
      shiny::req(savedImage())
      file.copy(savedImage(), file)
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
