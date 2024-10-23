jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Band-pass Filter", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      tags$head(tags$style(
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

                   /* Input with info button styling */
              .input-with-info {
                display: flex;
                align-items: center;
              }

                 .input-with-info label {
                margin-right: 5px;
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
                margin-bottom: 5px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 3px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }

                   #plotMeanSpectrum {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
               margin-right: 5px !important;
              }

                     #plotSpectro {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }
                 #saveEditedWave {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
              margin-right: 5px !important;
              }

                 #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 10px; /* Optional: Adjust padding */
              border-radius: 5px; /* Optional: Rounded corners */
              }
              "
        )
      )),

      shiny::fluidRow(
        shiny::column(2, shiny::selectInput("selectedWave", "Select a wave object:", choices = NULL, width = '100%')),
        shiny::column(2, shiny::verticalLayout(
          shiny::actionButton("plotMeanSpectrum", "Mean Spectrum"),
          shiny::actionButton("plotSpectro", "Spectrogram")
        )),
        shiny::column(1, shiny::verticalLayout(
          shiny::numericInput("highpass", "HPF (kHz)", value = 0, min = 0),
          shiny::numericInput("lowpass", "LPF (kHz)", value = 48, min = 1)
        )),
        shiny::column(1, shiny::actionButton("applyFilter", "Apply Filter")),
        shiny::column(2, shiny::verticalLayout(
          shiny::textInput("newName", "Name for new wave:", value = ""),
          shiny::actionButton("saveEditedWave", "Save")
        )),
        shiny::column(1, shiny::actionButton("close", "Close App"))
      ),
      shiny::fluidRow(
        shiny::column(12, shiny::div(style = "margin-top: 15px;",
                                     shinycssloaders::withSpinner(plotly::plotlyOutput("audioPlot", height = "520px", width = "1480px"))))
      )
    )
  )
}
