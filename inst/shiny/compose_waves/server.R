server <- function(input, output, session) {

  # Reactive value to store the timeline order (using a list to allow duplicates)
  timeline_order <- reactiveVal(character(0))

  # Reactive value to store the merged wave
  merged_wave_reactive <- reactiveVal(NULL)

  # Dynamically update the available wave objects from the global environment
  observe({
    wave_names <- ls(envir = .GlobalEnv)  # Get all objects in the global environment
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]  # Filter only Wave objects

    # Update the list of available wave objects
    output$wave_list <- renderUI({
      tagList(
        lapply(wave_names, function(wave_name) {
          div(
            class = "wave-item",
            id = wave_name,
            draggable = TRUE,
            ondragstart = "event.dataTransfer.setData('text/plain', event.target.id)",
            style = "padding: 10px; margin: 5px; border: 1px solid #ccc; cursor: grab;",
            wave_name
          )
        })
      )
    })
  })

  # Render the timeline horizontally
  output$timeline <- renderUI({
    div(
      id = "timeline",
      class = "timeline",
      style = "display: flex; flex-direction: row; overflow-x: auto; min-height: 100px; border: 2px dashed #ccc; padding: 10px; margin: 5px;",
      lapply(seq_along(timeline_order()), function(index) {
        wave_name <- timeline_order()[index]  # Get the wave name
        div(
          class = "wave-item",
          id = paste0("timeline_", index),  # Unique display ID based on position
          style = "padding: 10px; margin: 5px; border: 1px solid #ccc; cursor: grab; min-width: 100px; position: relative;",
          wave_name,  # Display the wave name
          span(
            class = "remove-wave",
            style = "position: absolute; top: 0; right: 0; cursor: pointer; color: red; font-weight: bold;",
            "×",  # "X" button
            onclick = paste0("Shiny.setInputValue('remove_wave', ", index, ")")  # Send index to server
          )
        )
      })
    )
  })

  # Observe drag-and-drop events for adding waves to the timeline
  observeEvent(input$wave_drop, {
    wave_name <- input$wave_drop
    timeline_order(c(timeline_order(), wave_name))  # Add the wave name to the timeline
  })

  # Observe remove-wave events
  observeEvent(input$remove_wave, {
    req(input$remove_wave)  # Ensure the index is provided
    index <- input$remove_wave  # Get the index of the wave to remove
    current_order <- timeline_order()  # Get the current timeline order
    if (index > 0 && index <= length(current_order)) {
      new_order <- current_order[-index]  # Remove the wave at the specified index
      timeline_order(new_order)  # Update the timeline order
    }
  })

  # Extract sampling rates of waves in the timeline
  sampling_rates <- reactive({
    req(timeline_order())  # Ensure there are waves in the timeline
    sapply(timeline_order(), function(wave_name) {
      wave <- get(wave_name, envir = .GlobalEnv)
      wave@samp.rate  # Extract sampling rate
    })
  })

  # Display sampling rates and allow the user to select one
  output$sampling_rate_ui <- renderUI({
    req(sampling_rates())  # Ensure sampling rates are available
    unique_rates <- unique(sampling_rates())  # Get unique sampling rates
    selectInput(
      "selected_sampling_rate",
      "Select Target Sampling Rate (Hz):",
      choices = unique_rates,
      selected = unique_rates[1]  # Default to the first unique rate
    )
  })

  # Checkbox for normalization
  normalize_checkbox <- reactiveVal(FALSE)

  # UI for the checkbox
  output$normalize_checkbox_ui <- renderUI({
    checkboxInput("normalize_checkbox", "Normalize Waves (24-bit depth)", value = normalize_checkbox())
  })

  # Merge waves when the merge button is clicked
  observeEvent(input$merge_button, {
    req(timeline_order(), input$selected_sampling_rate)  # Ensure waves and a sampling rate are selected

    # Fetch the selected Wave objects from the global environment
    selected_waves <- lapply(timeline_order(), function(wave_name) {
      wave <- get(wave_name, envir = .GlobalEnv)  # Fetch the wave object

      # Resample the wave if its sampling rate doesn't match the selected rate
      if (wave@samp.rate != input$selected_sampling_rate) {
        wave <- seewave::resamp(wave, g = as.numeric(input$selected_sampling_rate), output = "Wave")
      }

      # Normalize the wave if the checkbox is checked
      if (input$normalize_checkbox) {
        wave <- tuneR::normalize(wave, unit = "24")
      }

      return(wave)
    })

    # Merge the selected Wave objects
    merged_wave <- do.call(tuneR::bind, selected_waves)

    # Display the merged wave summary
    output$merged_wave_summary <- renderPrint({
      print(merged_wave)
    })

    # Save the merged wave to a reactive value
    merged_wave_reactive(merged_wave)
  })

  # Save the merged wave to the R environment
  observeEvent(input$save_button, {
    req(merged_wave_reactive())  # Ensure the merged wave object exists
    wave_name <- input$new_wave_name  # Get the name entered by the user
    assign(wave_name, merged_wave_reactive(), envir = .GlobalEnv)  # Save the merged wave to the global environment
    showNotification(paste("Wave object", wave_name, "saved to R memory"), type = "message")  # Notification
  })

  # Download handler for saving the merged wave as a .wav file
  output$download_merged_wave <- downloadHandler(
    filename = function() {
      paste0(input$new_wave_name, ".wav")
    },
    content = function(file) {
      req(merged_wave_reactive())  # Ensure the merged wave object exists
      tuneR::writeWave(merged_wave_reactive(), file)  # Write the merged wave object to a .wav file
      # showNotification(paste("Wave", input$new_wave_name, "downloaded successfully!"), type = "message")
      #
    }
  )

  # Stop the app when the session ends
  session$onSessionEnded(function() {
    stopApp()
  })
}
