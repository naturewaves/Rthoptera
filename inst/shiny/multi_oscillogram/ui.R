jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui = function(request) {
  tagList(
    shiny::h1("Multi-Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
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

          .btn-group-vertical > .btn {
            margin-bottom: 5px; /* Space between vertical buttons */
          }
          .row {
            margin-bottom: 5px; /* Vertical space between rows */
          }
          .shiny-input-container {
            margin-right: 2px; /* Horizontal space between inputs */
          }
          #plotMultiOscillogram {
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
        shiny::column(2, shiny::selectInput("waveObjects", "Select wave objects:", choices = NULL, selected = NULL, multiple = TRUE, width = '100%')),
        shiny::column(1, shiny::verticalLayout(
          shiny::numericInput("maxDur", "Max dur. (s):", value = 1, min = 0.5, step = 0.5, width = '80%')
        )),
        shiny::column(1, shiny::verticalLayout(
          shiny::numericInput("scalebarLength", "Scale bar length (s):", value = 0.1, min = 0.05, step = 0.01, width = '100%'),
          shiny::div(
            shiny::checkboxInput("allScalebar", "Add scale bar to each", value = FALSE),
            style = "font-size: 13px !important;"
          )
        )),
        shiny::column(1, shiny::numericInput("imgWidth", "PNG Width (in):", value = 16, min = 1)),
        shiny::column(1, shiny::numericInput("imgHeight", "PNG Height (in/oscillogram):", value = 2, min = 1)),
        shiny::column(2, shiny::actionButton("plotMultiOscillogram", "Plot multi-oscillogram")),
        shiny::column(2, shiny::verticalLayout(
          shiny::downloadButton("saveImage", "Save PNG", width = '100%'),
          shiny::div(style = "font-size: 13px !important;",
                     shiny::checkboxInput("transparentBg", "Save with \ntransparent background", value = FALSE)
          )
        )),
        shiny::column(2, shiny::actionButton("close", "Close App"))
      ),

      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput("plotOutput"))
      )

    )
  )
}

