#' Multi-Power Spectra Static Shiny App
#'
#' This Shiny app allows users to create multiple power spectra from selections in the oscillogram.
#' The app provides options to download individual or combined visualizations.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing wave oscillograms and mean spectra.
#'
#' @import shiny
#' @import patchwork
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom seewave meanspec
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select row_number pull
#' @importFrom shinycssloaders withSpinner
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   multi_meanspectra_static_app()
#' }
#' }

multi_meanspectra_static_app <- function(launch.browser = FALSE){

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Multi-Power Spectra", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        tags$head(tags$style(
          HTML(
            "
/* General body styling */

body {
  background-color: #252626;
  color: #ffffff;
  margin: 20px;
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

.btn-group-vertical > .btn {
  margin-bottom: 10px; /* Adds space between vertical buttons */
}
.row {
  margin-bottom: 10px; /* Adds vertical space between rows */
}
.shiny-input-container {
  margin-right: 10px !important;
}


/* Styling for dialog boxes */
.modal-dialog {
  border-radius: 10px !important; /* This applies rounding to the outer modal container */
}

.modal-content {
  background-color: #252626;
  color: #ffffff;
  border-radius: 15px !important; /* Rounded content container */
  overflow: hidden; /* Ensure content follows the rounded corners */
  box-shadow: 0 5px 15px rgba(0,0,0,.5); /* Optional: add a shadow */
}
.modal-header, .modal-footer {
  background-color: #343a40;
  color: #ffffff;
  border-top: none;
  border-bottom: none;
  border-radius: 15px 15px 0 0 !important;
}

.modal-footer {
  border-radius: 0 0 15px 15px !important; /* Round bottom corners */
}

.modal-body {
   background-color: #252626;
   color: #ffffff;
}

#audioPlot {
height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
width: 100%;
}
.btn-group-vertical > .btn {
margin-bottom: 10px; /* Adds space between vertical buttons */
}
.row {
margin-bottom: 10px; /* Adds vertical space between rows */
}
.shiny-input-container {
margin-right: 2px; /* Reduces horizontal space between inputs */
}

.download {
    margin-right: 5px;
    margin-bottom: 5px;
    border: 2px solid dodgerblue;
    border-radius: 5px;
     }

#close {
border: 2px solid red; /* Red contour */
padding: 5px 5px; /* Button (inside) padding */
border-radius: 5px; /* Optional: Rounded corners */
}


.container-fluid {
    max-width: 99%;
    max-height: 99%;
    margin-left: 20px;
    marging-right: 15px;
  }

"
          )
        ))),
      fluidRow(
        column(2, selectInput("wave_select", "Select a Wave Object:", choices = NULL)),
        column(1, verticalLayout(
          checkboxInput("show_total_mean", "Show Total Mean Spectrum", value = TRUE),
          numericInput("opacity", "Fill opacity:", value = 0.9, min = 0.1, max = 1, step = 0.1))
        ),
        column(1, selectInput("wl", "Window Length: ", selected = 4096, choices = c(512,1024,2048,4096,8192), width='80%')),
        column(1, actionButton("add_selection", "Add")),
        column(2,
               div(style = "margin-right: 5px;", textInput("file_name", "File prefix:", value = "", width = '70%'))
        ),
        column(2, div(downloadButton("download_together", "Download Plot",
                                     class = "download")
        )
        ),
        column(1, actionButton("close", "Close App", class = "btn-danger"))
      ),
      fluidRow(
        column(12, plotOutput("oscillogram", height = "150px", width = "100%",
                              brush = brushOpts(id = "wave_brush", direction = "x")))
      ),

      fluidRow(
        column(12, plotOutput("mean_spectrum", height = "350px", width = "100%"))
      )
    )
  }


server <- function(input, output, session) {

  # Functions for data processing
  wave_df <- function(wave){
    srate <- wave@samp.rate
    amplitude <- wave@left
    tbl <- tibble(amplitude = amplitude) %>%
      mutate(time = (row_number() - 1) / srate) %>%
      mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
    return(tbl)
  }

  createOscillogram <- function(wave, brush_data = list(), colors = NULL) {
    tbl <- wave_df(wave)

    p <- ggplot(tbl, aes(x = time, y = amplitude)) +
      geom_line(color = "black", size = 0.5) +
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
      labs(x = "Time (s)", y = "Amplitude") +
      scale_x_continuous(expand = c(0, 0))  # Remove padding at the beginning and end of x-axis

    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]
        selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
        p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude), color = colors[i], size = 0.5)
      }
    }

    return(p)
  }

  extract.meanspec <- function(wave, from = NULL, to = NULL, wl = 1024) {
    full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE, wl = wl, fftw = TRUE)
    tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
  }
  plot.meanspec <- function(wave, brush_data = list(), colors = NULL, opacity = 0.8, wl = 1024, show_total_mean = TRUE) {
    # Extract the full spectrum
    full_spec <- extract.meanspec(wave, wl = wl)

    p <- ggplot(full_spec, aes(x = frequency, y = amplitude)) +
      labs(x = "Frequency (kHz)", y = "Amplitude") +
      theme_minimal() +
      scale_x_continuous(expand = c(0, 0))  # Remove padding

    # Conditionally add the total mean spectrum line
    if (show_total_mean) {
      p <- p + geom_line(color = "black", size = 0.8, aes(x = frequency, y = amplitude), show.legend = TRUE)
    }

    # Add selections if they exist
    if (!is.null(colors) && length(brush_data) > 0) {
      for (i in seq_along(brush_data)) {
        range <- brush_data[[i]]

        # Calculate the length of the selection
        selection_length <- range[2] - range[1]

        # Validate the selection length for the window length
        if (selection_length * wave@samp.rate < wl) {
          shiny::showModal(modalDialog(
            title = "Selection Too Short",
            "Please provide a longer selection or choose a smaller window length.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)  # Exit early if selection is invalid
        }

        # Calculate and plot the spectrum for the selection
        spec <- extract.meanspec(wave, from = range[1], to = range[2], wl = wl)
        spec$amplitude <- spec$amplitude * max(wave_df(wave) %>%
                                                 dplyr::filter(time >= range[1] & time <= range[2]) %>%
                                                 pull(amplitude))
        p <- p + geom_line(data = spec, aes(x = frequency, y = amplitude), color = colors[i], size = 0.5) +
          geom_area(data = spec, aes(x = frequency, y = amplitude), fill = alpha(colors[i], opacity), color = NA)
      }
    }

    return(p)
  }

  # Reactive values to store the wave and brushed ranges
  selected_wave <- reactiveVal()
  brushed_ranges <- reactiveVal(list())

  # Reactive value to store the path of the temporary plot file
  combined_plot_file <- reactiveVal(NULL)

  observeEvent(input$wave_select, {
    if (input$wave_select != "") {
      wave_obj <- get(input$wave_select, envir = .GlobalEnv)
      selected_wave(wave_obj)
      brushed_ranges(list())
      combined_plot_file(NULL)
      updateTextInput(session, "file_name", value = isolate(input$wave_select))
    }
  })

  observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    updateSelectInput(session, "wave_select", choices = wave_names)
  })

  output$oscillogram <- renderPlot({
    req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")
    createOscillogram(wave, brush_data, colors)
  })

  output$mean_spectrum <- renderPlot({
    req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")
    plot.meanspec(wave, brush_data, colors, opacity = input$opacity, wl = as.numeric(input$wl), show_total_mean = input$show_total_mean)
  })

  observeEvent(input$add_selection, {
    req(input$wave_brush)
    brush <- input$wave_brush
    brush_data <- brushed_ranges()
    brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
    brushed_ranges(brush_data)
    combined_plot_file(NULL)  # Invalidate the cached plot file when selections change
  })

  # Generate combined plot and cache it in a temporary file
  observe({
    req(selected_wave())
    wave <- selected_wave()
    brush_data <- brushed_ranges()
    colors <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7")

    p1 <- createOscillogram(wave, brush_data, colors)
    p2 <- plot.meanspec(wave, brush_data, colors, wl = as.numeric(input$wl))

    # Combine the plots using patchwork
    combined_plot <- p1 / p2

    # Create a temporary file path to store the combined plot
    temp_file <- tempfile(fileext = ".png")

    # Save the combined plot to the temporary file
    ggsave(temp_file, plot = combined_plot, width = 20, height = 12, units = "in", dpi = 300)

    # Cache the file path
    combined_plot_file(temp_file)
  })

  output$download_together <- downloadHandler(
    filename = function() {
      paste0(input$file_name,"_multi_power_spectra.png")
    },
    content = function(file) {
      # Use the cached temporary file if available
      temp_file <- combined_plot_file()
      if (!is.null(temp_file)) {
        file.copy(temp_file, file)
      }
    }
  )

  # Stop the app when the session ends
  session$onSessionEnded(function() {
    stopApp()
  })

  # Stop the app when the close button is used
  observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    stopApp()
  })
}



if(launch.browser){

  shinyApp(ui = ui, server = server, options = list(launch.browser = browser))

} else {

  shinyApp(ui = ui, server = server)

}

}
