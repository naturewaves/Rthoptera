server <- function(input, output, session) {

  selected_wave <- shiny::reactiveVal()
  brushed_ranges <- shiny::reactiveVal(list())

  # Function to extract a data frame from a Wave object
  wave_df <- function(wave) {
    srate <- wave@samp.rate
    amplitude <- wave@left
    tbl <- tibble::tibble(amplitude = amplitude)
    tbl <- tbl %>%
      dplyr::mutate(index = dplyr::row_number(),
                    time = (index - 1) / srate) %>%
      dplyr::select(c(amplitude, time)) %>%
      dplyr::mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
    return(tbl)
  }

  # Function to plot an oscillogram from a wave using wave_df and ggplot
  oscillo_ggplot <- function(wave, brush_data = list(), colors = NULL) {
    tbl <- wave_df(wave)
    time_range <- range(tbl$time)

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
      ggplot2::scale_x_continuous(limits = c(time_range[1], time_range[2]), expand = c(0, 0)) +
      ggplot2::labs(x = "Time (s)", y = "Amplitude")

    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]
        selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
        p <- p + ggplot2::geom_line(data = selected_data, ggplot2::aes(x = time, y = amplitude),
                                    color = colors[i], linewidth = 0.5)
      }
    }

    return(p)
  }

  # Function to extract a tibble with meanspectrum information from seewave::meanspec
  spectrum_df <- function(wave, from = NULL, to = NULL, wl = as.numeric(input$wl)) {
    full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE,
                                   wl = wl, fftw = TRUE)
    full_spec_df <- tibble::tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
    return(full_spec_df)
  }

  meanspec_plotly <- function(wave, wl, title) {
    full_spec <- spectrum_df(wave, wl = as.numeric(wl))

    p <- plotly::plot_ly(full_spec, x = ~frequency, y = ~amplitude, type = 'scatter',
                         mode = 'lines',
                         line = list(color = 'black', shape = 'spline'),
                         name = 'Mean',
                         hovertemplate = 'Amplitude: %{y:.2f}'
    ) %>%
      plotly::config(displayModeBar = TRUE) %>%
      plotly::layout(
        hovermode = 'x',
        title = list(
          text = title,
          x = 0.3,
          xanchor = "left"
        ),
        showlegend = TRUE,
        xaxis = list(
          title = list(text = "Frequency (Hz)", standoff = 10),
          ticklen = 5,
          automargin = TRUE,
          zeroline = FALSE,
          showline = TRUE
        ),
        yaxis = list(title = "Amplitude",
                     rangemode = "tozero",
                     ticklen = 5,
                     showline = TRUE),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.1,
          xanchor = "right"
        ),
        margin = list(
          l = 50,
          r = 10,
          b = 60,
          t = 50
        )
      )
    return(p)
  }

  # Automatic color stacking using a colorblind-safe palette
  brush_colors <- shiny::reactiveVal(c("#0072B2", "#E69F00", "#009E73", "#CC79A7",
                                       "#F0E442", "#56B4E9", "#999999", "#D55E00"))

  plotly_obj <- shiny::reactiveVal()

  shiny::observeEvent(input$wave_select, {
    if (input$wave_select != "") {
      wave_obj <- get(input$wave_select, envir = .GlobalEnv)
      selected_wave(wave_obj)
      brushed_ranges(list())
      shiny::updateTextInput(session, "file_name", value = shiny::isolate(input$wave_select))
    }
  })

  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "wave_select", choices = wave_names)
  })

  output$oscillogram <- shiny::renderPlot({
    shiny::req(input$plot_button)
    shiny::req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- brush_colors()
    p <- oscillo_ggplot(wave, brush_data, colors)
    p
  })

  output$mean_spectrum <- plotly::renderPlotly({
    shiny::req(input$plot_button)
    shiny::req(selected_wave())
    wave <- selected_wave()
    p <- meanspec_plotly(wave,
                         wl = as.numeric(shiny::isolate(input$wl)),
                         title = shiny::isolate(input$plot_title))
    plotly_obj(p)
    p
  })

  shiny::observeEvent(input$add_selection, {
    shiny::req(input$wave_brush)

    selection_name <- if (input$selection_choice == "Custom...") {
      input$custom_selection_name
    } else {
      input$selection_choice
    }

    brush <- input$wave_brush
    brush$xmin <- round(brush$xmin, 3)
    brush$xmax <- round(brush$xmax, 3)

    if (brush$xmin >= brush$xmax) {
      shiny::showModal(shiny::modalDialog(
        title = "Invalid Selection",
        "The selection is invalid. Please try again.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return(NULL)
    }

    brush_data <- brushed_ranges()
    brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
    brushed_ranges(brush_data)

    wave <- selected_wave()
    range <- c(brush$xmin, brush$xmax)

    spec <- spectrum_df(wave, from = range[1], to = range[2], wl = as.numeric(shiny::isolate(input$wl)))
    spec$amplitude <- spec$amplitude * max(wave_df(wave) %>%
                                             dplyr::filter(time >= range[1] & time <= range[2]) %>%
                                             dplyr::pull(amplitude))

    colors <- brush_colors()

    plotly::plotlyProxy("mean_spectrum", session) %>%
      plotly::plotlyProxyInvoke("addTraces", list(
        x = spec$frequency, y = spec$amplitude, type = 'scatter',
        mode = 'lines',
        line = list(shape = 'spline', color = 'transparent'),
        fill = 'tozeroy',
        fillcolor = plotly::toRGB(colors[length(brush_data)], alpha = shiny::isolate(input$alpha)),
        name = selection_name,
        hovertemplate = 'Amplitude: %{y:.2f}'
      ))

    p <- plotly_obj()
    p <- p %>%
      plotly::add_trace(x = spec$frequency, y = spec$amplitude, type = 'scatter', mode = 'none',
                        fill = 'tozeroy', fillcolor = plotly::toRGB(colors[length(brush_data)], alpha = shiny::isolate(input$alpha)),
                        name = selection_name,
                        hovertemplate = 'Amplitude: %{y:.2f}', line = list(color = 'rgba(0,0,0,0)'))
    plotly_obj(p)
  })

  output$download_oscillogram <- shiny::downloadHandler(
    filename = function() {
      paste0(input$file_name, "_oscillogram.png")
    },
    content = function(file) {
      shiny::req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- brush_colors()
      p <- oscillo_ggplot(wave, brush_data, colors)

      ggplot2::ggsave(filename = file, plot = p, device = "png",
                      width = 20, height = 4,
                      units = "in", dpi = 300, bg = "white")
    }
  )

  output$download_power_spectra <- shiny::downloadHandler(
    filename = function() {
      paste0(input$file_name, "_meanpowerspectra.html")
    },
    content = function(file) {
      shiny::req(plotly_obj())
      htmlwidgets::saveWidget(plotly_obj(), file, selfcontained = TRUE)
    }
  )

  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}
