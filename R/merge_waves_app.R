#' Shiny App to Merge Wave Objects
#'
#' This function launches a Shiny app that allows users to select multiple `Wave` objects from the R environment, merge them over time,
#' and save the merged object back to R memory or download it as a `.wav` file.
#'
#' @return A Shiny app for merging `Wave` objects
#' @export
#' @import shiny
#' @importFrom tuneR writeWave Wave
#' @examples
#' if (interactive()) {
#'   merge_waves_app()
#' }
merge_waves_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Merge Waves", style = "font-size: 28px; margin-left: 15px;"),
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
        ))),

      fluidRow(
        column(3,
               selectInput("wave_select", "Select Waves:", choices = NULL, multiple = TRUE)
               ),
        column(2,
               textInput("new_wave_name", "Enter Name for New Wave", value = "merged_wave")
               ),
        column(1,
               actionButton("merge_button", "Merge Wave")
               ),
        column(1, div(actionButton("save_button", "Save to R", class = 'download'))
               ),
        column(2, div(
               downloadButton("download_merged_wave", "Download", class = 'download'))
               ),
        column(1,
               actionButton("close", "Close App", class = "btn-danger"))

      ),

      mainPanel(
        h3("Merged Wave Object"),
        verbatimTextOutput("merged_wave_summary")
      )
    )


  }

  # Define the Shiny server
  server <- function(input, output, session) {

    # Dynamically update the available wave objects from the global environment
    observe({
      wave_names <- ls(envir = .GlobalEnv)  # Get all objects in the global environment
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]  # Filter only Wave objects
      updateSelectInput(session, "wave_select", choices = wave_names)  # Update the selectInput choices
    })

    # Reactive value to store the merged wave object
    merged_wave <- reactiveVal(NULL)

    # Observe when the merge button is clicked
    observeEvent(input$merge_button, {
      req(input$wave_select)  # Ensure at least one wave is selected

      # Fetch the selected Wave objects from the global environment
      selected_waves <- lapply(input$wave_select, function(wave_name) {
        get(wave_name, envir = .GlobalEnv)
      })

      # Merge the selected Wave objects
      merged_wave(merge_waves(selected_waves))

      # Display the merged wave summary
      output$merged_wave_summary <- renderPrint({
        req(merged_wave())
        print(merged_wave())  # Print the summary of the merged wave object
      })
    })

    # Save the merged wave to the R environment when the "Save Wave to R Memory" button is clicked
    observeEvent(input$save_button, {
      req(merged_wave())  # Ensure the merged wave object exists
      wave_name <- input$new_wave_name  # Get the name entered by the user
      assign(wave_name, merged_wave(), envir = .GlobalEnv)  # Save the merged wave to the global environment
      showNotification(paste("Wave object", wave_name, "saved to R memory"), type = "message")  # Notification
    })

    # Download handler for saving the merged wave as a .wav file
    output$download_merged_wave <- downloadHandler(
      filename = function() {
        paste0(input$new_wave_name, ".wav")
      },
      content = function(file) {
        req(merged_wave())  # Ensure the merged wave object exists
        writeWave(merged_wave(), file)  # Write the merged wave object to a .wav file
      }
    )

    # Stop the app when the session ends
    session$onSessionEnded(function() {
      stopApp()
    })

    # Stop the app when the close button is used
    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })

  }

  # Run the Shiny app
  shinyApp(ui = ui, server = server)
}
