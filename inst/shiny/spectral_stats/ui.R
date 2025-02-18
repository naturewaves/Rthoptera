jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {

  shiny::tagList(
    shiny::h1("Spectral Statistics",
              style = "font-size: 28px; margin-left: 15px; margin-top: 0px;
                       margin-bottom: 2px; margin-right: 15px;"),

    shiny::fluidPage(
      title = "Spectral Statistics",
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

          #run {
            border: 2px solid forestgreen; /* Green contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Rounded corners */
          }

          #saveDataEnv, #downloadData, #savePlot {
            border: 2px solid dodgerblue; /* Blue contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Rounded corners */
            margin-bottom: 5px !important; /* Space below the button */
          }

          #close {
            border: 2px solid red; /* Red contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Rounded corners */
          }

          .btn:hover, .btn-info:hover {
            background-color: #5a6268;
            border-color: #5a6268;
          }

          /* Styling for popovers */
          .popover {
            background-color: #ffffff;
            border: 1px solid #252626;
            color: #252626;
          }

          /* DataTables Styling */
          .dataTables_wrapper .caption-top {
            caption-side: top !important;
            font-weight: bold;
            color: #ffffff;
          }

          .dataTables_wrapper .dataTables_length,
          .dataTables_wrapper .dataTables_filter,
          .dataTables_wrapper .dataTables_info,
          .dataTables_wrapper .dataTables_paginate,
          .dataTables_wrapper .dataTables_processing {
            color: #ffffff;
          }

          .dataTable thead th,
          .dataTable tfoot th {
            color: #ffffff;
            border-color: #ffffff;
          }

          .dataTable tbody td {
            color: #ffffff;
            border-color: #ffffff;
          }

          /* Ensure horizontal lines in tables are white */
          .dataTable tbody tr {
            border-top: 1px solid #ffffff;
            border-bottom: 1px solid #ffffff;
          }

          /* Input with info button styling */
          .input-with-info {
            display: flex;
            align-items: center;
          }

          .input-with-info label {
            margin-right: 5px;
          }
          "
        )
      )),

      shiny::column(2,
                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::selectInput("selectedWave", "Select a Wave Object:",
                                                       choices = NULL)
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Specimen ID"),
                                      shinyBS::bsButton("specimen_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::textInput("specimen.id", label = NULL, value = "")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "specimen_info",
                      title = "Specimen ID",
                      content = shiny::HTML(paste0("A unique identifier for the specimen.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),


                    # shiny::column(12, shiny::textInput("locName", "Locality", value = "")),
                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Locality"),
                                      shinyBS::bsButton("locality_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::textInput("locality", label = NULL, value = "")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "locality_info",
                      title = "Locality",
                      content = shiny::HTML(paste0("The place where the specimen was found.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),


                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Sound Type"),
                                      shinyBS::bsButton("soundtype_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::textInput("sound.type", label = NULL, value = "Calling song")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "soundtype_info",
                      title = "Sound Type",
                      content = shiny::HTML(paste0("The type of sound under analysis.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Temperature (C)"),
                                      shinyBS::bsButton("temp_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::numericInput("temp",
                                                        label = NULL,
                                                        value = NA,
                                                        min = 0,
                                                        max = 60,
                                                        step = 0.1)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "temp_info",
                      title = "Temperature",
                      content = shiny::HTML(paste0("The ambient temperature at the moment of the recording, in degrees Celcius.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("HPF (kHz)"),
                                      shinyBS::bsButton("hpf_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::numericInput("hpf",
                                                        label = NULL,
                                                        value = 0,
                                                        min = 0,
                                                        max = 15,
                                                        step = 1)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "hpf_info",
                      title = "High Pass Filter",
                      content = shiny::HTML(paste0("If a High-Pass Filter was applied to the Wave before the analysis, inform which frequency was used, in kHz.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Scale"),
                                      shinyBS::bsButton("scale_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::selectInput("scale",
                                                       label = NULL,
                                                       choices = list("db", "linear"),
                                                       selected = "linear")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "scale_info",
                      title = "Scale",
                      content = shiny::HTML(paste0("Select either decibel or linear scale. Both scales are normalized by the maximum amplitude value in the Wave. The ranges are [0:1] for the linear scale and [-(min):0] for the decibel scale.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Threshold"),
                                      shinyBS::bsButton("threshold_info", label = "", lib="font-awesome",
                                                        shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::numericInput("cutoff",
                                                        label = NULL,
                                                        value = 0.4,
                                                        min = 0.05,
                                                        max = 0.95,
                                                        step = 0.05)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "threshold_info",
                      title = "Threshold",
                      content = shiny::HTML(paste0("Select an amplitude threshold to be used for the calculation of the frequency bandwidth. A common threshold is -20 dB below the peak, which in the linear scale is equivalent to 0.1.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    )
      ),

      shiny::column(10,
                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::column(2,
                                                  shiny::fluidRow(
                                                    shiny::column(8, shiny::checkboxInput("total", "Total Bandwidth", value = FALSE)),
                                                    shiny::column(4, shinyBS::bsButton("total_bandwidth_info", label = "", lib="font-awesome",
                                                                                       shiny::icon("circle-info"), style = "margin-left: 1px;",
                                                                                       size = "extra-small", class = "btn-info"))
                                                  ),
                                                  shiny::fluidRow(
                                                    shiny::column(8, shiny::checkboxInput("robust", "Robust", value = TRUE)),
                                                    shiny::column(4, shinyBS::bsButton("robust_info", label = "", lib="font-awesome",
                                                                                       shiny::icon("circle-info"), style = "default",
                                                                                       size = "extra-small", class = "btn-info"))
                                                  )
                                    ),
                                    shiny::column(2, shiny::actionButton("run", "Run Analysis")),
                                    shiny::column(2, shiny::verticalLayout(
                                      shiny::actionButton("saveDataEnv", "Table to R"),
                                      shiny::downloadButton("downloadData", "Export CSV"),
                                      shiny::downloadButton("savePlot", "Save Plot")
                                    )),
                                    shiny::column(1, shiny::actionButton("close", "Close App", style = 'white-space: nowrap;'))
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "total_bandwidth_info",
                      title = "Total Bandwidth",
                      content = shiny::HTML("This option uses the first and last samples (from left to right) above the amplitude threshold to assess the frequency bandwidth of the signal, regardless of the gaps (i.e., where the sampled frequencies go below the threshold) that might occur between the extremes and the peak frequency."),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),
                    shinyBS::bsPopover(
                      id = "robust_info",
                      title = "Robust",
                      content = shiny::HTML("This option performs a robust analysis, where the frequency resolution is fixed at ~244.1 Hz, intended to reflect broad spectral structure, thus ignoring subtle differences within and between individuals."),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "margin-top: 10px; margin-left: 10px; margin-right: 10px;",
                                               shinycssloaders::withSpinner(shiny::uiOutput("plotOutput")))
                      )
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shinycssloaders::withSpinner(DT::DTOutput("dataOutput"))
                      )
                    )
      )
    )
  )
}
