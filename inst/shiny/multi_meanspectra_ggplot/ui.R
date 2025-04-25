# JavaScript code to customize the "close" button
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Multi-Power Spectra", style = "font-size: 28px; margin-left: 15px;"),
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      shiny::tags$head(shiny::tags$style(
        shiny::HTML(
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
      )),

      shiny::fluidRow(
        shiny::column(2, shiny::selectInput("wave_select", "Select a Wave Object:", choices = NULL)),
        shiny::column(1, shiny::verticalLayout(
          shiny::checkboxInput("show_total_mean", "Show Total Mean Spectrum", value = TRUE),
          shiny::checkboxInput("norm_each", "Normalize Each", value = TRUE),
          shiny::numericInput("opacity", "Fill opacity:", value = 0.9, min = 0.1, max = 1, step = 0.1)
        )),
        shiny::column(1, shiny::selectInput("wl", "Window Length: ", selected = 4096, choices = c(512, 1024, 2048, 4096, 8192), width = '80%')),
        shiny::column(1,
                      shiny::verticalLayout(
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::actionButton("plot_button", "Plot",
                                                       class = "btn-space")
                        ),
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::actionButton("add_selection",
                                                       "Add Selection",
                                                       class = "btn-space")
                        )
                      )
        ),
        shiny::column(2,
                      shiny::div(style = "margin-right: 5px;", shiny::textInput("file_name", "File prefix:", value = "", width = '70%'))
        ),
        shiny::column(2, shiny::div(shiny::downloadButton("download_together", "Download Plot", class = "download"))),
        shiny::column(1, shiny::actionButton("close", "Close App", class = "btn-danger"))
      ),
      shiny::fluidRow(
        shiny::column(12, shiny::plotOutput("oscillogram", height = "150px", width = "100%", brush = shiny::brushOpts(id = "wave_brush", direction = "x")))
      ),
      shiny::fluidRow(
        shiny::column(12, shiny::plotOutput("mean_spectrum", height = "350px", width = "100%"))
      )
    )
  )
}


