server <- function(input, output, session) {

  # Functions for data processing
  wave_df <- function(wave) {
    srate <- wave@samp.rate
    amplitude <- wave@left
    tbl <- tibble::tibble(amplitude = amplitude) %>%
      dplyr::mutate(time = (dplyr::row_number() - 1) / srate) %>%
      dplyr::mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
    return(tbl)
  }

  createOscillogram <- function(wave, brush_data = list(), colors = NULL) {
    tbl <- wave_df(wave)

    p <- ggplot2::ggplot(tbl, ggplot2::aes(x = time, y = amplitude)) +
      ggplot2::geom_line(color = "black", linewidth = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black")
      ) +
      ggplot2::expand_limits(y = c(-1.2, 1.2)) +
      ggplot2::labs(x = "Time (s)", y = "Amplitude") +
      ggplot2::scale_x_continuous(expand = c(0, 0))  # Remove padding at the beginning and end of x-axis

    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]
        selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
        p <- p + ggplot2::geom_line(data = selected_data, ggplot2::aes(x = time, y = amplitude), color = colors[i], linewidth = 0.5)
      }
    }

    return(p)
  }

  extract_meanspec <- function(wave, from = NULL, to = NULL, wl = 1024) {
    full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE, wl = wl, fftw = TRUE)
    tibble::tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
  }

  plot_meanspec <- function(wave, brush_data = list(), colors = NULL, opacity = 0.8, wl = 1024, show_total_mean = TRUE) {
    full_spec <- extract_meanspec(wave, wl = wl)

    p <- ggplot2::ggplot(full_spec, ggplot2::aes(x = frequency, y = amplitude)) +
      ggplot2::labs(x = "Frequency (kHz)", y = "Amplitude") +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(expand = c(0, 0))  # Remove padding

    if (show_total_mean) {
      p <- p + ggplot2::geom_line(color = "black", linewidth = 0.8, ggplot2::aes(x = frequency, y = amplitude))
    }

    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]
        spec <- extract_meanspec(wave, from = range[1], to = range[2], wl = wl)
        p <- p + ggplot2::geom_line(data = spec, ggplot2::aes(x = frequency, y = amplitude), color = colors[i], size = 0.5) +
          ggplot2::geom_area(data = spec, ggplot2::aes(x = frequency, y = amplitude), fill = scales::alpha(colors[i], opacity), color = NA)
      }
    }

    return(p)
  }

  selected_wave <- shiny::reactiveVal()
  brushed_ranges <- shiny::reactiveVal(list())

  combined_plot_file <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$wave_select, {
    if (input$wave_select != "") {
      wave_obj <- get(input$wave_select, envir = .GlobalEnv)
      selected_wave(wave_obj)
      brushed_ranges(list())
      combined_plot_file(NULL)
      shiny::updateTextInput(session, "file_name", value = shiny::isolate(input$wave_select))
    }
  })

  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "wave_select", choices = wave_names)
  })

  output$oscillogram <- shiny::renderPlot({
    shiny::req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")
    createOscillogram(wave, brush_data, colors)
  })

  output$mean_spectrum <- shiny::renderPlot({
    shiny::req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")
    plot_meanspec(wave, brush_data, colors, opacity = input$opacity, wl = as.numeric(input$wl), show_total_mean = input$show_total_mean)
  })

  shiny::observeEvent(input$add_selection, {
    shiny::req(input$wave_brush)
    brush <- input$wave_brush
    brush_data <- brushed_ranges()
    brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
    brushed_ranges(brush_data)
    combined_plot_file(NULL)
  })

  shiny::observe({
    shiny::req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")

    p1 <- createOscillogram(wave, brush_data, colors)
    p2 <- plot_meanspec(wave, brush_data, colors, wl = as.numeric(input$wl))

    combined_plot <- p1 / p2

    temp_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(temp_file, plot = combined_plot, width = 20, height = 12, units = "in", dpi = 300)
    combined_plot_file(temp_file)
  })

  output$download_together <- shiny::downloadHandler(
    filename = function() {
      paste0(input$file_name, "_multi_power_spectra.png")
    },
    content = function(file) {
      temp_file <- combined_plot_file()
      if (!is.null(temp_file)) {
        file.copy(temp_file, file)
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
