#' Multi-Mean Spectra Shiny App
#'
#' This Shiny app allows users to visualize the mean spectra of multiple selections from an audio waveform. Users can interactively select regions of a waveform, generate their respective mean spectra, and overlay these spectra using a colorblind-safe palette. The app also provides functionality to download the oscillogram and mean spectra as image files.
#'
#'@param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing and downloading multi-selection mean spectra and oscillograms.
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom htmltools HTML
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom dplyr mutate filter pull row_number
#' @importFrom tibble tibble
#' @importFrom seewave meanspec
#' @importFrom plotly plot_ly add_trace plotlyProxy plotlyProxyInvoke toRGB
#' @importFrom htmlwidgets saveWidget
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   multi_meanspectra_app()
#' }
#' }
multi_meanspectra_app <- function(launch.browser = FALSE) {

  # Javascript code to customize the "close" button
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Multi-Mean Spectra", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        tags$head(
          tags$style(
            HTML(
              "
                /* General body styling */
                body {
                  background-color: #252626;
                  color: #ffffff;
                  margin: 5px;
                }

                /* Styling for the inputs */
                .form-control {
                  background-color: #495057;
                  border: 1px solid #6c757d;
                  color: #ffffff;
                }

                 .btn-info {
                  background-color: #252626 !important;
                  border-color: #252626 !important;
                  color: #ffffff;
                }

                /* Styling for buttons */
                .btn {
                  background-color: #343a40;
                  border-color: #6c757d;
                  color: #ffffff;
                }

                .row {
                  margin-bottom: 10px; /* Adds vertical space between rows */
                }

                /* Consistent button layout */
                .btn-space {
                  margin-right: 5px;
                  border: 2px solid green;
                }

                .btn-down {
                  margin-right: 5px;
                  border: 2px solid dodgerblue;
                }

                   #close {
                border: 2px solid red; /* Red contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }

                /* Adjust container for better fit */
                .container-fluid {
                  max-width: 99%;
                  max-height: 99%;
                  padding-left: 5px;
                  padding-right: 5px;
                }
              "
            )
          )
        ),

        fluidRow(
          column(2,
                 div(style = "margin-right: 5px;", selectInput("wave_select", "Select a Wave Object:", choices = NULL, width = '100%'))
          ),
          column(1,
                 div(style = "margin-right: 5px;", selectInput("wl", "Window Length: ", selected = 4096, choices = c(512,1024,2048,4096,8192), width='90%'))
          ),
          column(2,
                 verticalLayout(
                   div(style = "margin-right: 5px;", textInput("plot_title", "Plot Title:", value = "", width = '100%')),
                   div(style = "margin-right: 5px;",
                       selectInput("selection_choice", "Selection Name:",
                                   choices = c("closing", "opening", "male", "female", "Custom..."),
                                   selected = "closing", width = '100%')
                   ),
                   conditionalPanel(
                     condition = "input.selection_choice == 'Custom...'",
                     div(style = "margin-right: 5px;",
                         textInput("custom_selection_name", "Custom Name:", value = "", width = '100%'))
                   )
                 )
          ),
          column(1,
                 verticalLayout(
                   div(style = "margin-right: 5px;", actionButton("plot_button", "Plot", class = "btn-space")),
                   div(style = "margin-right: 5px;", actionButton("add_selection", "Add Selection", class = "btn-space"))
                 )
          ),
          column(1,
                 div(style = "margin-right: 5px;", numericInput("alpha", "Opacity", value = 0.9, min = 0.1, max = 1, step = 0.1), width='60%')
          ),
          column(2,
                 div(style = "margin-right: 5px;", textInput("file_name", "File prefix:", value = "", width = '70%'))
          ),
          column(2,
                 verticalLayout(
                   div(style = "margin-bottom: 5px;", downloadButton("download_oscillogram", "Download Oscillogram", class = "btn-down")),
                   div(style = "margin-bottom: 5px;", downloadButton("download_power_spectra", "Download Power Spectra", class = "btn-down")),
                   div(style = "margin-right: 5px;", actionButton("close", "Close App"))
                 )
          )
        ),

        fluidRow(
          column(12,
            plotOutput("oscillogram", height = "150px", width = "100%",
                       brush = brushOpts(id = "wave_brush", direction = "x"))
            ),
          column(12,
            withSpinner(plotlyOutput("mean_spectrum", height = "350px", width = "100%"))
            )
        )
      )
    )
  }

  server <- function(input, output, session) {

    selected_wave <- reactiveVal()
    brushed_ranges <- reactiveVal(list())

    # Function to extract a data frame from a Wave object
    wave_df <- function(wave){
      srate <- wave@samp.rate
      amplitude <- wave@left
      tbl <- tibble(amplitude = amplitude)
      tbl <- tbl %>%
        mutate(index = row_number(),
               time = (index - 1) / srate) %>%
        select(c(amplitude, time)) %>%
        mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
      return(tbl)
    }

    # Function to plot an oscillogram from a wave using wave_df and ggplot
    oscillo_ggplot <- function(wave, brush_data = list(), colors = NULL) {
      tbl <- wave_df(wave)
      time_range <- range(tbl$time)

      p <- ggplot(tbl, aes(x = time, y = amplitude)) +
        geom_line(color = "black", linewidth = 0.5) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_line(color = "black")
        ) +
        expand_limits(y = c(-1.2, 1.2)) +
        scale_x_continuous(limits = c(time_range[1], time_range[2]), expand = c(0,0)) +
        labs(x = "Time (s)", y = "Amplitude")

      if (!is.null(colors) && length(brush_data) > 0) {
        for (i in seq_along(brush_data)) {
          range <- brush_data[[i]]
          selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
          p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude),
                             color = colors[i], linewidth = 0.5)
        }
      }

      return(p)
    }

    # Function to extract a tibble with meanspectrum information from seewave::meanspec
    spectrum_df <- function(wave, from = NULL, to = NULL, wl = as.numeric(input$wl)) {
      full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE,
                                     wl = wl, fftw = TRUE)
      full_spec_df <- tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
      return(full_spec_df)
    }

    meanspec_plotly <- function(wave, wl, title) {
      full_spec <- spectrum_df(wave, wl = as.numeric(wl))

      p <- plot_ly(full_spec, x = ~frequency, y = ~amplitude, type = 'scatter',
                   mode = 'lines',
                   line = list(color = 'black', shape = 'spline'),
                   name = 'Mean',
                   hovertemplate = 'Amplitude: %{y:.2f}'
      ) %>%
        config(displayModeBar = TRUE) %>%
        layout(
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
    brush_colors <- reactiveVal(c("#0072B2","#E69F00","#009E73", "#CC79A7",

                                  "#F0E442", "#56B4E9", "#999999","#D55E00" ))

    plotly_obj <- reactiveVal()

    observeEvent(input$wave_select, {
      if (input$wave_select != "") {
        wave_obj <- get(input$wave_select, envir = .GlobalEnv)
        selected_wave(wave_obj)
        brushed_ranges(list())
        updateTextInput(session, "file_name", value = isolate(input$wave_select))
      }
    })

    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "wave_select", choices = wave_names)
    })

    output$oscillogram <- renderPlot({
      req(input$plot_button)
      req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- brush_colors()
      p <- oscillo_ggplot(wave, brush_data, colors)
      p
    })

    output$mean_spectrum <- renderPlotly({
      req(input$plot_button)
      req(selected_wave())
      wave <- selected_wave()
      p <- meanspec_plotly(wave,
                           wl = as.numeric(isolate(input$wl)),
                           title = isolate(input$plot_title))
      plotly_obj(p)
      p
    })

    observeEvent(input$add_selection, {
      req(input$wave_brush)

      # Get the selected name or custom name
      selection_name <- if (input$selection_choice == "Custom...") {
        input$custom_selection_name
      } else {
        input$selection_choice
      }

      # Round xmin and xmax to avoid floating-point precision issues
      brush <- input$wave_brush
      brush$xmin <- round(brush$xmin, 3)
      brush$xmax <- round(brush$xmax, 3)

      # Validate that xmin < xmax
      if (brush$xmin >= brush$xmax) {
        showModal(modalDialog(
          title = "Invalid Selection",
          "The selection is invalid. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }

      brush_data <- brushed_ranges()
      brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
      brushed_ranges(brush_data)

      wave <- selected_wave()
      range <- c(brush$xmin, brush$xmax)

      spec <- spectrum_df(wave, from = range[1], to = range[2], wl = as.numeric(isolate(input$wl)))
      spec$amplitude <- spec$amplitude * max(wave_df(wave) %>%
                                               dplyr::filter(time >= range[1] & time <= range[2]) %>%
                                               pull(amplitude))

      colors <- brush_colors()
      # selection_name <- isolate(input$selection_name)

      plotlyProxy("mean_spectrum", session) %>%
        plotlyProxyInvoke("addTraces", list(
          x = spec$frequency, y = spec$amplitude, type = 'scatter',
          mode = 'lines',
          line = list(shape = 'spline', color = 'transparent'),
          fill = 'tozeroy',
          fillcolor = toRGB(colors[length(brush_data)], alpha = isolate(input$alpha)),
          name = selection_name,
          hovertemplate = 'Amplitude: %{y:.2f}'
        ))

      # Update plotly object with new trace
      p <- plotly_obj()
      p <- p %>%
        add_trace(x = spec$frequency, y = spec$amplitude, type = 'scatter', mode = 'none',
                  fill = 'tozeroy', fillcolor = toRGB(colors[length(brush_data)], alpha = isolate(input$alpha)),
                  name = selection_name,
                  hovertemplate = 'Amplitude: %{y:.2f}', line = list(color = 'rgba(0,0,0,0)'))
      plotly_obj(p)
    })

    output$download_oscillogram <- downloadHandler(
      filename = function() {
        paste0(input$file_name,"_oscillogram.png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- brush_colors()
        p <- oscillo_ggplot(wave, brush_data, colors)

        ggsave(filename = file, plot = p, device = "png",
               width = 20, height = 4,
               units = "in", dpi = 300, bg="white")
      }
    )

    output$download_power_spectra <- downloadHandler(

      filename = function() {
        paste0(input$file_name,"_meanpowerspectra.html")
      },
      content = function(file) {
        req(plotly_obj())
        saveWidget(plotly_obj(), file, selfcontained = TRUE)
      }
    )


    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })

    session$onSessionEnded(function() {
      stopApp()
    })
  }

  if(launch.browser){
    shinyApp(ui = ui, server = server, options = list(launch.browser = browser))
  } else {
    shinyApp(ui = ui, server = server)
  }
}
