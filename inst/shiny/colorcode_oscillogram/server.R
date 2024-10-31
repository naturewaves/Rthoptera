server <- function(input, output, session) {

  createOscillogram <- function(wave, brush_data = list(), colors = NULL,
                                show_axes = TRUE, add_scale_bar = FALSE,
                                scale_bar_size = 1, scale_bar_position_pct = 0) {
    tbl <- wave_df(wave)
    time_range <- range(tbl$time)
    scale_bar_position <- time_range[1] + (scale_bar_position_pct / 100) * (time_range[2] - time_range[1])

    p <- ggplot2::ggplot(tbl, ggplot2::aes(x = time, y = amplitude)) +
      ggplot2::geom_line(color = "black", size = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 10, l = 5)
      ) +
      ggplot2::expand_limits(y = c(-1.2, 1.2)) +
      ggplot2::scale_x_continuous(limits = c(time_range[1], time_range[2]), expand = c(0,0)) +
      ggplot2::labs(x = "Time (s)", y = "Amplitude")

    if (!show_axes) {
      p <- p + ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    } else {
      p <- p + ggplot2::theme(
        axis.line = ggplot2::element_line(color = "black")
      )
    }

    if (add_scale_bar) {
      scale_bar_label <- ifelse(scale_bar_size < 1, paste0(scale_bar_size * 1000, " ms"), paste0(scale_bar_size, " s"))
      p <- p + ggplot2::annotate("segment", x = scale_bar_position, xend = scale_bar_position + scale_bar_size, y = -1.1, yend = -1.1, colour = "black", size = 1) +
        ggplot2::annotate("text", x = scale_bar_position + scale_bar_size / 2, y = -1.25, label = scale_bar_label, size = 4, vjust = 1)
    }

    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]
        selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
        p <- p + ggplot2::geom_line(data = selected_data, ggplot2::aes(x = time, y = amplitude),
                                    color = colors[i], size = 0.5)
      }
    }

    return(p)
  }

  selected_wave <- shiny::reactiveVal()
  brushed_ranges <- shiny::reactiveVal(list())
  brush_colors <- shiny::reactiveVal(character())

  shiny::observeEvent(input$wave_select, {
    if (input$wave_select != "") {
      wave_obj <- get(input$wave_select, envir = .GlobalEnv)
      selected_wave(wave_obj)
      brushed_ranges(list())
      brush_colors(character())
    }
  })

  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "wave_select", choices = wave_names)
  })

  output$oscillogram <- shiny::renderPlot({
    shiny::req(input$plot_button)
    shiny::req(input$height)
    shiny::req(input$width)
    shiny::req(input$dpi)
    shiny::req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- brush_colors()
    max_position_pct <- 100 - (input$scale_bar_size / diff(range(wave_df(wave)$time))) * 100
    shiny::updateSliderInput(session, "scale_bar_position", max = max_position_pct)

    show_axes <- input$toggle_option == "show_axes"
    add_scale_bar <- input$toggle_option == "add_scale_bar"

    p <- createOscillogram(wave, brush_data, colors, show_axes, add_scale_bar, input$scale_bar_size, input$scale_bar_position)
    p
  })

  shiny::observeEvent(input$add_selection, {
    shiny::req(input$wave_brush)
    brush <- input$wave_brush
    brush_data <- brushed_ranges()
    brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
    brushed_ranges(brush_data)

    colors <- brush_colors()
    colors <- append(colors, input$color_picker)
    brush_colors(colors)
  })

  output$download_oscillogram <- shiny::downloadHandler(
    filename = function() {
      paste0("_coded_oscillogram.png")
    },
    content = function(file) {
      shiny::req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- brush_colors()
      max_position_pct <- 100 - (input$scale_bar_size / diff(range(wave_df(wave)$time))) * 100
      shiny::updateSliderInput(session, "scale_bar_position", max = max_position_pct)

      show_axes <- input$toggle_option == "show_axes"
      add_scale_bar <- input$toggle_option == "add_scale_bar"

      p <- createOscillogram(wave, brush_data, colors, show_axes, add_scale_bar, input$scale_bar_size, input$scale_bar_position)

      ggplot2::ggsave(filename = file, plot = p, device = "png",
                      width = input$width, height = input$height,
                      units = "in", dpi = shiny::isolate(input$dpi), bg = "white")
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
