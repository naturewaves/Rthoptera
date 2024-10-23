jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Trim", style = "font-size: 28px; margin-left: 15px;"),
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

            /* Styling for dialog boxes */
            .modal-dialog {
              border-radius: 10px !important;
            }

            .modal-content {
              background-color: #252626;
              color: #ffffff;
              border-radius: 15px !important;
              overflow: hidden;
              box-shadow: 0 5px 15px rgba(0,0,0,.5);
            }
            .modal-header, .modal-footer {
              background-color: #343a40;
              color: #ffffff;
              border-top: none;
              border-bottom: none;
              border-radius: 15px 15px 0 0 !important;
            }

            .modal-footer {
              border-radius: 0 0 15px 15px !important;
            }

            .modal-body {
               background-color: #252626;
               color: #ffffff;
            }

            #audioPlot {
              height: calc(100vh - 120px);
              width: 100%;
            }
            .btn-group-vertical > .btn {
              margin-bottom: 5px;
            }
            .row {
              margin-bottom: 3px;
            }
            .shiny-input-container {
              margin-right: 2px;
            }

            #plot {
             border: 2px solid forestgreen;
             border-radius: 5px;
            }

            #saveSelection {
              border: 2px solid dodgerblue;
              border-radius: 5px;
            }

            #close {
              border: 2px solid red;
              padding: 5px 10px;
              border-radius: 5px;
            }
          "
        )
      )),

      shiny::fluidRow(
        shiny::column(2,
                      shiny::selectInput("selectedWave", "Select a wave object:",
                                         choices = NULL, width = '100%')
        ),
        shiny::column(1,
                      shiny::actionButton("plot", "Plot")
        ),
        shiny::column(3, shiny::verticalLayout(
          shiny::actionButton("zoomIn", "Zoom In"),
          shiny::actionButton("zoomOut", "Zoom Out")
        )),
        shiny::column(3, shiny::verticalLayout(
          shiny::textInput("selectionName", "Name for the selection:"),
          shiny::actionButton("saveSelection", "Save Selection")
        )),
        shiny::column(1, shiny::actionButton("close", "Close App"))
      ),

      shiny::fluidRow(
        shiny::column(12,
                      shiny::div(style = "margin-top: 15px;",
                                 shinycssloaders::withSpinner(shiny::plotOutput("wavePlot",
                                                                                brush = shiny::brushOpts(id = "waveBrush", direction = "x"),
                                                                                height = "520px", width = "1480px"))))
      )
    )
  )
}
