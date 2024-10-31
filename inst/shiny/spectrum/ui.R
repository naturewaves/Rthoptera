ui <- function(request){
  shiny::tagList(
    shiny::h1("Spectrum", style = "font-size: 28px; margin-left: 15px;"),
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

            /* Sidebar panel background styling */
            .sidebar {
              background-color: #343a40; /* Dark gray background */
              border-color: #343a40; /* Optional: border color to match */
            }

            /* Adjust font color inside the sidebar */
            .sidebar .form-control, .sidebar {
              color: #ffffff;
            }

            /* Styling for wellPanel (if used inside the sidebar) */
            .well {
              background-color: #343a40;
              border: none;
              color: #ffffff;
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

            #specPlot {
              height: calc(100vh - 120px);
              width: 100%;
            }

            .btn-group-vertical > .btn {
              margin-bottom: 10px;
            }

            .row {
              margin-bottom: 10px;
            }

            .shiny-input-container {
              margin-right: 2px;
            }

            #plot {
              border: 2px solid forestgreen; /* Blue contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }

            #downloadPlot {
              border: 2px solid dodgerblue; /* Blue contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }

            #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }
            "
          )
        )
      ),

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("waveObject", "Select a wave object:", choices = NULL),
          shiny::textInput("plotTitle", "Plot title:", value = ""),
          shiny::checkboxInput("italicTitle", "Italicize Title", value = TRUE),
          shiny::selectInput("wl", "Window Length:", selected = 4096,
                             choices = c(128, 256, 512, 1024, 2048, 4096, 8192)),
          shiny::numericInput("ovlp", "Overlap (%):", value = 75, min = 0, max = 100),
          shiny::selectInput("wn", "Window Function:",
                             choices = c("bartlett", "blackman", "flattop",
                                         "hamming", "hanning", "rectangle"),
                             selected = "hanning"),
          shiny::selectInput("scale", "Scale:", choices = c("linear", "dB", "dBFS")),
          shiny::conditionalPanel(
            condition = "input.scale == 'dB' || input.scale == 'dBFS'",
            shiny::numericInput("yMin", "Minimum amplitude (dB):", value = -100),
            shiny::textInput("yBreaks", "Amplitude Ticks:", value = "-40, -20, 0")
          ),
          width = 2
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(2, shiny::verticalLayout(
              shiny::selectInput("yPosition", "Y-axis Position:", choices = c("left", "right")),
              shiny::selectInput("xPosition", "X-axis Position:", choices = c("bottom", "top"))
            )),
            shiny::column(2, shiny::verticalLayout(
              shiny::checkboxInput("showXTitle", "Show X-axis Title", value = TRUE),
              shiny::checkboxInput("showYTitle", "Show Y-axis Title", value = TRUE),
              shiny::checkboxInput("addParams", "Add Parameters", value = TRUE),
              shiny::checkboxInput("flip", "Flip Axes", value = FALSE)
            )),
            shiny::column(2, shiny::verticalLayout(
              shiny::textInput("fill", "Fill Color:", value = "black"),
              shiny::selectInput("fun", "Function:", choices = c("mean", "median", "var", "sd"))
            )),
            shiny::column(2, shiny::verticalLayout(
              shiny::numericInput("plotWidth", "Width (in):", value = 15, min = 1),
              shiny::numericInput("plotHeight", "Height (in):", value = 5, min = 1)
            )),
            shiny::column(1, shiny::actionButton("plot", "Plot Spectrum")),
            shiny::column(1, shiny::downloadButton("downloadPlot", "Save PNG")),
            shiny::column(1, shiny::actionButton("close", "Close App"))
          ),
          shiny::fluidRow(
            shinycssloaders::withSpinner(shiny::plotOutput("plot"))
          ),
          shiny::fluidRow(
            shiny::column(2, shiny::numericInput("fmin", "Min. Freq:",
                                                 value = 0, min =0, max=100, step = 1)),
            shiny::column(2, shiny::numericInput("fmax", "Max. Freq:",
                                                 value = 48, min =10, max=193, step = 1)),
            shiny::column(2, shiny::numericInput("x.breaks", "Nr of Frequency Ticks:",
                                                 value = 8, min = 4, max = 20))
          ),
          width = 10
        )
      )
    )
  )
}
