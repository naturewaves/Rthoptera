jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Merge Waves", style = "font-size: 28px; margin-left: 15px;"),
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

          .btn-group-vertical > .btn {
            margin-bottom: 10px; /* Adds space between vertical buttons */
          }

          .row {
            margin-bottom: 10px; /* Adds vertical space between rows */
          }

          .shiny-input-container {
            margin-right: 2px; /* Reduces horizontal space between inputs */
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

          #merge_button {
            border: 2px solid forestgreen; /* Green contour */
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
        shiny::column(3,
                      shiny::selectInput("wave_select", "Select Waves:", choices = NULL, multiple = TRUE)
        ),
        shiny::column(2,
                      shiny::textInput("new_wave_name", "Enter Name for New Wave", value = "merged_wave")
        ),
        shiny::column(1,
                      shiny::actionButton("merge_button", "Merge Wave")
        ),
        shiny::column(1,
                      shiny::div(shiny::actionButton("save_button", "Save to R", class = 'download'))
        ),
        shiny::column(2,
                      shiny::div(shiny::downloadButton("download_merged_wave", "Download", class = 'download'))
        ),
        shiny::column(1,
                      shiny::actionButton("close", "Close App", class = "btn-danger")
        )
      ),

      shiny::mainPanel(
        shiny::h3("Merged Wave Object"),
        shiny::verbatimTextOutput("merged_wave_summary")
      )
    )
  )
}
