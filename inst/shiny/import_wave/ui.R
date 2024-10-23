jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui = function(request) {
  shiny::tagList(
    shiny::h1("Import Wave", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
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
       /* Styling for the fileInput text (file name) */

 /* Correct target for file input text color */
    .form-control {
      background-color: #495057;
      border: 1px solid #6c757d;
      color: #333333 !important; /* Dark grey for file name text */
    }

    /* Specifically targeting the 'Name for new wave' input */
    #newName {
      color: #ffffff !important; /* White text for the 'Name for new wave' input */
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

          .btn-group-vertical > .btn {
            margin-bottom: 5px; /* Adds space between vertical buttons */
          }
          .row {
            margin-bottom: 10px; /* Adds vertical space between rows */
          }
          #saveEditedWave {
            border: 2px solid dodgerblue; /* Blue contour */
            border-radius: 5px; /* Optional: Rounded corners */
          }
          #close {
            border: 2px solid red; /* Red contour */
            border-radius: 5px; /* Optional: Rounded corners */
          }
          .aligned-row {
            display: flex;
            align-items: left;
          }
          .aligned-row > div {
            margin-right: 15px; /* Adds consistent spacing between columns */
          }
          .aligned-row > div:last-child {
            margin-right: 0; /* Removes right margin for the last element */
          }
          "
        )
      )),
      shiny::fluidRow(
        shiny::column(3, shiny::div(class = "aligned-row",
                                    shiny::fileInput(
                                      "audioFile",
                                      "Choose an audio file",
                                      accept = c("audio/wav", ".wav", ".mp3", ".wac"))
        )),
        shiny::column(3, shiny::div(class = "aligned-row",
                                    shiny::textInput("newName", "Name for new wave:", value = ""))
        ),
        shiny::column(2, shiny::div(class = "aligned-row",
                                    shiny::actionButton("saveEditedWave", "Import Audio"))
        ),
        shiny::column(2, shiny::div(class = "aligned-row",
                                    shiny::actionButton("close", "Close App"))
        )
      )
    )
  )

}
