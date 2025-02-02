jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Song Statistics LQ", style = "font-size: 28px; margin-left: 15px;"),
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

            #run {
             border: 2px solid forestgreen;
             padding: 5px 5px;
             border-radius: 5px;
            }

            #saveData {
              border: 2px solid dodgerblue;
              padding: 5px 5px;
              border-radius: 5px;
            }

            #savePlot {
              border: 2px solid dodgerblue;
              padding: 5px 5px;
              border-radius: 5px;
            }

            #close {
              border: 2px solid red;
              padding: 5px 5px;
              border-radius: 5px;
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

            .dataTable tbody tr {
              border-top: 1px solid #ffffff;
              border-bottom: 1px solid #ffffff;
            }

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
                                    shiny::selectInput("preset", label = NULL,
                                                       choices = c("Tettigoniidae", "Gryllidae"),
                                                       selected = "Tettigoniidae")
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
                      content = shiny::HTML(paste0("A unique identifier for the specimen. For example, &#39GRYCAM_001&#39, is the &#39Alpha code&#39 for Gryllus campestris, specimen 001.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Detection Threshold"),
                                                 shinyBS::bsButton("detection_threshold_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("detection_threshold",
                                                        label = NULL,
                                                        value = 0.01,
                                                        min = 0.001,
                                                        max = 1,
                                                        step = 0.001)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "detection_threshold_info",
                      title = "Detection Threshold",
                      content = shiny::HTML(paste0("Set the threshold for detecting peaks. Any peak below this value will be discarded.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Smoothing"),
                                                 shinyBS::bsButton("ssmooth_window_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("ssmooth", label = NULL, value = 100,
                                                        min = 10, max = 1000, step = 10)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "ssmooth_window_info",
                      title = "Smoothing",
                      content = shiny::HTML(paste0("Window size (samples) used to smooth the envelope. A larger window will result in a smoother envelope.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Peakfinder Window"),
                                                 shinyBS::bsButton("peakfinder_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("peakfinder_ws",
                                                        label = NULL,
                                                        value = 40,
                                                        min = 10,
                                                        max = 200,
                                                        step = 5)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "peakfinder_info",
                      title = "Peakfinder Window",
                      content = shiny::HTML(paste0("Window size (samples) used to find peaks along the envelope.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Peakfinder Threshold"),
                                                 shinyBS::bsButton("peakfinder_thr_info", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("peakfinder_threshold",
                                                        label = NULL,
                                                        value = 0.005,
                                                        min = 0.001,
                                                        max = 0.5,
                                                        step = 0.001)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "peakfinder_thr_info",
                      title = "Peakfinder Threshold",
                      content = shiny::HTML(paste0("The minimum distance between a valley and a peak. This distance is measured as a proportion relative to the maximum amplitude [0:1].")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Max Peak Gap"),
                                                 shinyBS::bsButton("max_peak", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("max_peak_gap", label = NULL, value = 0.01,
                                                        min = 0.001, max = 0.1, step = 0.001)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "max_peak",
                      title = "Max Peak Gap",
                      content = shiny::HTML(paste0("The maximum gap (in seconds) allowed between peaks to be considered as belonging to the same train.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),

                    shiny::fluidRow(
                      shiny::column(12,
                                    shiny::div(style = "display: flex; align-items: center;",
                                               shiny::tagList(
                                                 shiny::tags$label("Max Train Gap"),
                                                 shinyBS::bsButton("max_train", label = "", lib = "font-awesome",
                                                                   icon = shiny::icon("circle-info"), style = "default",
                                                                   size = "extra-small", class = "btn-info")
                                               )
                                    ),
                                    shiny::numericInput("max_train_gap", label = NULL, value = 0.08,
                                                        min = 0.01, max = 1, step = 0.01)
                      )
                    ),
                    shinyBS::bsPopover(
                      id = "max_train",
                      title = "Max Train Gap",
                      content = shiny::HTML(paste0("The maximum gap (in seconds) allowed between trains to be grouped in the same motif.")),
                      placement = "right",
                      trigger = "click",
                      options = list(container = "body")
                    ),


                    shiny::conditionalPanel(
                      condition = "input.motif_seq == true",
                      shiny::fluidRow(
                        shiny::column(12,
                                      shiny::div(style = "display: flex; align-items: center;",
                                                 shiny::tagList(
                                                   shiny::tags$label("Max Motif Gap (s)"),
                                                   shinyBS::bsButton("max_motif_gap_info", label = "", lib = "font-awesome",
                                                                     icon = shiny::icon("circle-info"), style = "default",
                                                                     size = "extra-small", class = "btn-info")
                                                 )
                                      ),
                                      shiny::numericInput("max_motif_gap",
                                                          label = NULL,
                                                          value = 1,
                                                          min = 0.01, max = 5,
                                                          step = 0.01)
                        )
                      ),
                      shinyBS::bsPopover(
                        id = "max_motif_gap_info",
                        title = "Max Motif Gap",
                        content = shiny::HTML(paste0("Maximum gap allowed between motifs to be considered in the same sequence If the gap exceeds this value, a new sequence is initialized")),
                        placement = "right",
                        trigger = "click",
                        options = list(container = "body")
                      )
                    ),


      ),

      shiny::column(10,
                    shiny::fluidRow(
                      shiny::column(2, shiny::actionButton("run", "Run Analysis"),
                                    shiny::checkboxInput("motif_seq", "Motif Sequences", value = TRUE)
                      ),
                      shiny::column(2, shiny::downloadButton("saveData", "Export Excel Workbook")),
                      shiny::column(2, shiny::downloadButton("savePlot", "Export HTML Plot")),
                      shiny::column(1, shiny::actionButton("close", "Close App")),
                      style = "margin-bottom: 20px;"
                    ),
                    shiny::fluidRow(
                      shiny::column(12,
                                    shinycssloaders::withSpinner(plotly::plotlyOutput("audioPlot")),
                                    DT::DTOutput("summary_data"),
                                    shiny::conditionalPanel(
                                      condition = "input.motif_seq == true",
                                      DT::DTOutput("motif_seq_data"),
                                    ),
                                    DT::DTOutput("motif_data"),
                                    DT::DTOutput("train_data"),
                                    DT::DTOutput("peak_data"),
                                    DT::DTOutput("params")
                      )
                    )
      )
    )
  )
}
