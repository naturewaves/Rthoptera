jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  tagList(
    shiny::h1("Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
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

            /* Input with info button styling */
            .input-with-info {
              display: flex;
              align-items: center;
            }

            .input-with-info label {
              margin-right: 5px;
            }


          #wavePlot, #zoomedPlot, #finalZoomedPlot {
            height: calc(100vh - 200px); /* Adjusts height taking into account other elements */
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

           #oscillogram {
             border: 2px solid forestgreen; /* Green contour */
             border-radius: 5px; /* Optional: Rounded corners */
           }

            #zoomedPortion {
             border: 2px solid forestgreen; /* Green contour */
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

          "
        )
      )),
      shiny::fluidRow(
        shiny::column(2,
                      shiny::selectInput("waveObject", "Select a wave object:", choices = NULL, selected = NULL, width = '100%'),
        ),
        shiny::column(1,
                      shiny::radioButtons("displayOption", "Display Option:", choices = c("Axes" = "axes", "Scale Bar" = "scalebar"), selected = "axes")
        ),
        shiny::column(2,
                      shiny::verticalLayout(
                        shiny::tags$style(shiny::HTML(".shiny-input-container { margin-bottom: 2px; }")),
                        shiny::numericInput("scaleBar1", "Scale Bar 1 (ms):", value = 500, min = 1, step = 100),
                        shiny::numericInput("scaleBar2", "Scale Bar 2 (ms):", value = 50, min = 1, step = 10),
                        shiny::numericInput("scaleBar3", "Scale Bar 3 (ms):", value = 10, min = 1, step = 1)
                      )
        ),
        shiny::column(1, shiny::numericInput("imgWidth", "Image Width (in):", value = 16, min = 1)),
        shiny::column(1, shiny::numericInput("imgHeight", "Image Height (in):", value = 2, min = 1)),
        shiny::column(2, shiny::verticalLayout(
          shiny::actionButton("oscillogram", "Plot Oscillogram"),
          shiny::actionButton("zoomedPortion", "Add Selection to Stack")
        )),
        shiny::column(2,
                      shiny::downloadButton("saveImage", "Save PNG"),
                      shiny::checkboxInput("transparentBg", "Save with transparent background", value = FALSE)
        ),
        shiny::column(1, shiny::actionButton("close", "Close App"))
      ),
      shiny::mainPanel(
        shiny::uiOutput("wavePlotUI"),
        shiny::uiOutput("zoomedPlotUI"),
        shiny::uiOutput("finalZoomedPlotUI")
      )
    )
  )
}

