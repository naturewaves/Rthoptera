server <- function(input, output, session) {

  # Dynamically update the available wave objects from the global environment
  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)  # Get all objects in the global environment
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]  # Filter only Wave objects
    shiny::updateSelectInput(session, "wave_select", choices = wave_names)  # Update the selectInput choices
  })

  # Reactive value to store the merged wave object
  merged_wave <- shiny::reactiveVal(NULL)

  # Observe when the merge button is clicked
  shiny::observeEvent(input$merge_button, {
    shiny::req(input$wave_select)  # Ensure at least one wave is selected

    # Fetch the selected Wave objects from the global environment
    selected_waves <- lapply(input$wave_select, function(wave_name) {
      get(wave_name, envir = .GlobalEnv)
    })

    # Merge the selected Wave objects
    merged_wave(merge_waves(selected_waves))

    # Display the merged wave summary
    output$merged_wave_summary <- shiny::renderPrint({
      shiny::req(merged_wave())
      print(merged_wave())  # Print the summary of the merged wave object
    })
  })

  # Save the merged wave to the R environment when the "Save Wave to R Memory" button is clicked
  shiny::observeEvent(input$save_button, {
    shiny::req(merged_wave())  # Ensure the merged wave object exists
    wave_name <- input$new_wave_name  # Get the name entered by the user
    assign(wave_name, merged_wave(), envir = .GlobalEnv)  # Save the merged wave to the global environment
    shiny::showNotification(paste("Wave object", wave_name, "saved to R memory"), type = "message")  # Notification
  })

  # Download handler for saving the merged wave as a .wav file
  output$download_merged_wave <- shiny::downloadHandler(
    filename = function() {
      paste0(input$new_wave_name, ".wav")
    },
    content = function(file) {
      shiny::req(merged_wave())  # Ensure the merged wave object exists
      tuneR::writeWave(merged_wave(), file)  # Write the merged wave object to a .wav file
    }
  )

  # Stop the app when the session ends
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  # Stop the app when the close button is used
  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

}
