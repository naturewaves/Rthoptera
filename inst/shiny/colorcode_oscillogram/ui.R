jscode <- "shinyjs.closeWindow = function() { window.close(); }"

colorblind_safe_palette <- c(
  "Orange" = "#E69F00",
  "Sky Blue" = "#56B4E9",
  "Bluish Green" = "#009E73",
  "Yellow" = "#F0E442",
  "Blue" = "#0072B2",
  "Vermillion" = "#D55E00",
  "Reddish Purple" = "#CC79A7",
  "Light Green" = "#8DD3C7",
  "Dark Purple" = "#BEBADA",
  "Light Orange" = "#FB8072"
)

ui <- fluidPage(
  titlePanel("Color-code Oscillogram"),
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
          .js-irs-0 .irs-grid,
          .js-irs-0 .irs-min,
          .js-irs-0 .irs-max,
          .js-irs-0 .irs-single,
          .js-irs-0 .irs-grid-pol{
            display: none !important; /* Hides slider labels and ticks */
          }
          .slider-container .shiny-input-container {
            width: 100% !important; /* Ensure slider takes full width */
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


           #plot_button {
               border: 2px solid forestgreen; /* Blue contour */
               padding: 5px 5px; /* Button (inside) padding */
               border-radius: 5px; /* Optional: Rounded corners */
              }
              #download_oscillogram {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }
              #close {
                border: 2px solid red; /* Red contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
                margin-left: 10px;
              }
          "
      )
    ))),

  shiny::fluidRow(
    shiny::column(2, shiny::selectInput("wave_select", "Select a Wave Object:", choices = NULL, selected = NULL)),
    shiny::column(1, shiny::actionButton("plot_button", "Plot")),
    shiny::column(1, colourpicker::colourInput("color_picker", "Select Color", value = "#E69F00",
                                               showColour = "background",
                                               palette = "limited",
                                               allowedCols = colorblind_safe_palette)),
    shiny::column(1, shiny::actionButton("add_selection", "Paint Selection")),
    shiny::column(2, shiny::verticalLayout(
      shiny::radioButtons("toggle_option", "Display Options:",
                          choices = list("Show Axes" = "show_axes", "Add Scale Bar" = "add_scale_bar"),
                          selected = "show_axes"),
      shiny::numericInput("scale_bar_size", "Scale Bar Size (s)", value = 0.1, min = 0.1, step = 0.1)
    )),
    shiny::column(1, shiny::verticalLayout(
      shiny::numericInput("height", "Height (in):", value = 2, step = 1),
      shiny::numericInput("width", "Width (in):", value = 10, step = 1)
    )),
    shiny::column(1, shiny::numericInput("dpi", "DPI", value = 200, step = 10)),
    shiny::column(1, shiny::downloadButton("download_oscillogram", "Download PNG")),
    shiny::column(1, shinyjs::useShinyjs(),
                  shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
                  shiny::actionButton("close", "Close App"))
  ),
  shiny::fluidRow(
    shiny::column(12, shinycssloaders::withSpinner(
      shiny::plotOutput("oscillogram", height = "180px", width = "1450px",
                        brush = shiny::brushOpts(id = "wave_brush", direction = "x"))
    ))
  ),
  shiny::fluidRow(
    shiny::column(12, shiny::div(class = "slider-container",
                                 shiny::sliderInput("scale_bar_position", "Scale bar position:",
                                                    min = 0, max = 100, value = 0, step = 1, ticks = FALSE)))
  )
)
