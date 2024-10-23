jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Spectrogram", style = "font-size: 28px; margin-left: 15px;"),
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
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

            /* Input with info button styling */
            .input-with-info {
              display: flex;
              align-items: center;
            }

            .input-with-info label {
              margin-right: 5px;
            }

            .btn-group-vertical > .btn {
              margin-bottom: 5px; /* Space between vertical buttons */
            }
            .row {
              margin-bottom: 10px; /* Vertical space between rows */
            }
            .shiny-input-container {
              margin-right: 2px; /* Horizontal space between inputs */
            }
            #plotSpectro {
              border: 2px solid forestgreen; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
            }
            #saveImage {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
            }
            #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 10px; /* Optional: Adjust padding */
              border-radius: 5px; /* Optional: Rounded corners */
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
            "
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(2,
                      shiny::selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%')
        ),
        shiny::column(1, shiny::verticalLayout(
          shiny::checkboxInput("meanspec", "Mean Spectrum", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.meanspec == true",
            shiny::selectInput("meanspecScale", "Scale:", selected = "linear",
                               choices = c("linear", "dB"))
          )
        )
        ),
        shiny::column(1,
                      shiny::numericInput("noise.floor", "Cutoff (dB)",
                                          value = -50, min = -80, max = -20, step = 5)
        ),
        shiny::column(1,
                      shiny::numericInput("overlap", "Overlap (%):",
                                          value = 95, min = 25, max = 95, step = 5)
        ),
        shiny::column(1,
                      shiny::numericInput("imgWidth", "Width (in):", value = 15, min = 1, step = 1)
        ),
        shiny::column(1,
                      shiny::numericInput("imgHeight", "Height (in):", value = 3, min = 1, step = 1)
        ),
        shiny::column(1,
                      shiny::actionButton("plotSpectro", "Plot")),
        shiny::column(2,
                      shiny::verticalLayout(
                        shiny::downloadButton("saveImage", "Save PNG"),
                        shiny::div(style = "font-size: 13px !important;",
                                   shiny::checkboxInput("transparentBg", "Transparent Background", value = FALSE)
                        )
                      )
        ),
        shiny::column(1, shiny::actionButton("close", "Close App"))
      ),

      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput("specPlotOutput"))
      )
    )
  )
}
