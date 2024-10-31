jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  tagList(
    shiny::h1("Temporal Statistics HQ", style = "font-size: 28px; margin-left: 15px;"),
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
             border: 2px solid forestgreen; /* Blue contour */
             padding: 5px 5px; /* Button (inside) padding */
             border-radius: 5px; /* Optional: Rounded corners */
            }

            #saveData {
              border: 2px solid dodgerblue; /* Blue contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }

            #savePlot {
              border: 2px solid dodgerblue; /* Blue contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }

            #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
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
                                    shiny::selectInput("selectedWave", "Select a Wave Object:", choices = NULL)
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::tagList(
                                      shiny::tags$label("Presets"),
                                      shinyBS::bsButton("preset_info", label = "", lib = "font-awesome",
                                                        icon = shiny::icon("circle-info"), style = "default",
                                                        size = "extra-small", class = "btn-info")
                                    ),
                                    shiny::selectInput("preset", label = NULL, choices = c("Tettigoniidae", "Gryllidae"), selected = "Gryllidae")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "preset_info",
                      title = "Preset",
                      content = shiny::HTML(paste0("Optimized parameters for call patterns in particular taxa.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Specimen ID"),
                                                 shinyBS::bsButton("specimen_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::textInput("specimen_id", label = NULL, value = "")
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "specimen_info",
                      title = "Specimen ID",
                      content = shiny::HTML(paste0("The Specimen ID is used to identify the individual specimen in your analysis.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Smoothing Window (points)"),
                                                 shinyBS::bsButton("msmooth_window_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("msmooth_window", label = NULL, value = 100,
                                                        min = 10, max = 1000, step = 10)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "msmooth_window_info",
                      title = "Smoothing Window Length",
                      content = shiny::HTML(paste0("Window size (samples) used to smooth the envelope. A larger window will result in a smoother envelope.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Overlap (%)"),
                                                 shinyBS::bsButton("msmooth_overlap_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("msmooth_overlap", label = NULL, value = 50, min = 0, max = 100, step = 5)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "msmooth_overlap_info",
                      title = "Window Overlap",
                      content = shiny::HTML(paste0("Overlap percentage between successive windows during smoothing. Higher overlap results in more smoothing.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Upper Detection Threshold"),
                                                 shinyBS::bsButton("upDet_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("upper_detection_threshold",
                                                        label = NULL,
                                                        value = 0.2,
                                                        min = 0.01,
                                                        max = 0.99,
                                                        step = 0.01)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "upDet_info",
                      title = "Upper Detection Threshold",
                      content = shiny::HTML(paste0("Minimum amplitude (proportion) required for a train to be included in the analysis. Trains with maximum amplitude below this value will be excluded.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Lower Detection Threshold"),
                                                 shinyBS::bsButton("loDet_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("lower_detection_threshold", label = NULL,
                                                        value = 0.1,
                                                        min = 0.01,
                                                        max = 0.99,
                                                        step = 0.01)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "loDet_info",
                      title = "Lower Detection Threshold",
                      content = shiny::HTML(paste0("Amplitude threshold as a proportion of the maximum amplitude. Only trains with an amplitude above this threshold will be detected.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Max Train Gap (s)"),
                                                 shinyBS::bsButton("max_train_gap_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("max_train_gap",
                                                        label = NULL,
                                                        value = 0.08,
                                                        min = 0.01, max = 1,
                                                        step = 0.01)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "max_train_gap_info",
                      title = "Max Train Gap",
                      content = shiny::HTML(paste0("Maximum gap allowed between trains to be considered in the same motif. If the gap exceeds this value, a new motif is started.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),
      ),

      shiny::column(10,
                    shiny::fluidRow(
                      shiny::column(2, shiny::actionButton("run", "Run Analysis")),
                      shiny::column(2, shiny::downloadButton("saveData", "Export Excel Workbook")),
                      shiny::column(2, shiny::verticalLayout(
                        shiny::downloadButton("savePlot", "Export HTML Plot"),
                        # shiny::checkboxInput("show_annotations", "Show Annotations", value = TRUE)  # Add this checkbox
                      )),
                      shiny::column(1, shiny::actionButton("close", "Close App"))
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::h3("NOTICE: This function resamples Wave objects to 192 kHz for consistent
            minimum resolution of time measurements. Sampling rates equal or higher are not resampled. See documentation for details.",
                                              style = "font-size: 12px; color: lightgrey;"),
                                    shinycssloaders::withSpinner(plotly::plotlyOutput("audioPlot")),
                                    DT::DTOutput("summary_data"),
                                    DT::DTOutput("motif_data"),
                                    DT::DTOutput("train_data"),
                                    # DT::DTOutput("gap_data"),
                                    DT::DTOutput("params"),
                                    style = "padding: 10px;"
                      )
                    )
      )
    )
  )
}
