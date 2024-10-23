jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui = function(request) {
  shiny::tagList(
    shiny::h1("Downsample", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      # theme = bslib::bs_theme(bootswatch = "darkly"),
      shiny::tags$head(shiny::tags$style(
        shiny::HTML(
          "
             body {
                background-color: #252626;
                color: #ffffff;
                margin: 5px;
             }

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

                  .modal-content {
      background-color: #252626;
      color: #ffffff;
    }
    .modal-header, .modal-footer {
      background-color: #343a40;
      color: #ffffff;
      border-bottom: 1px solid #6c757d;
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
                margin-right: 10px; /* Increases horizontal space between inputs */
              }

               #plotMeanSpectrum {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }

                 #saveEditedWave {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
                 }

                 #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 10px; /* Optional: Adjust padding */
              border-radius: 5px; /* Optional: Rounded corners */
              }

              "
        )
      )),
      shiny::tags$script(
        shiny::HTML(
          "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
              "
        )
      ),

      shiny::fluidRow(
        shiny::column(3,
                      shiny::div(class = 'inline', shiny::selectInput("selectedWave", "Select a wave object:", choices = NULL, width = '90%'))
                      # div(style = "margin-top: 5px;", shiny::actionButton("refresh", "Refresh", width='70%'))
        ),
        shiny::column(1,
                      shiny::div(style = "margin-top: 20px;", shiny::actionButton("plotMeanSpectrum", "Plot"))
        ),
        shiny::column(2,
                      shiny::div(class = 'inline', shiny::selectInput('maxfreq', 'New Max. Freq. (kHz):', choices = c(48, 96, 125))),
                      shiny::div(style = "margin-top: 5px;", shiny::actionButton("downsample", "Downsample"))
        ),

        shiny::column(2,
                      shiny::div(class = 'inline', shiny::textInput("newName", "Name for new wave:", value = "")),
                      shiny::div(style = "margin-top: 5px;", shiny::actionButton("saveEditedWave", "Save"))
        ),

        shiny::column(1,
                      shiny::div(style = "margin-top: 5px;", shiny::actionButton("help", "Help"))
        ),

        shiny::column(1, shiny::actionButton("close", "Close App"))
      ),

      shiny::fluidRow(
        shiny::column(12,
                      shiny::div(style = "margin-top: 15px;",
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("audioPlot", height = "520px", width = "1480px"))))
      )
    )
  )
}
