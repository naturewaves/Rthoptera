server <- function(input, output, session) {

  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "selectedWave", choices = wave_names)
  })

  result <- shiny::eventReactive(input$run, {
    shiny::req(input$selectedWave)
    wave <- get(input$selectedWave, envir = .GlobalEnv)
    temporal_stats_hq(wave,
                      specimen_id = shiny::isolate(input$specimen_id),
                      msmooth_window = as.numeric(shiny::isolate(input$msmooth_window)),
                      msmooth_overlap = as.numeric(shiny::isolate(input$msmooth_overlap)),
                      upper_detection_threshold = as.numeric(shiny::isolate(input$upper_detection_threshold)),
                      lower_detection_threshold = as.numeric(shiny::isolate(input$lower_detection_threshold)),
                      min_train_dur = as.numeric(0.002),
                      max_train_gap = as.numeric(shiny::isolate(input$max_train_gap)),
                      norm_env = TRUE)
  })

  shiny::observeEvent(input$preset, {

    if (input$preset == "Gryllidae") {

      shiny::updateTextInput(session, "specimen_id", value = "")
      shiny::updateNumericInput(session, "msmooth_window", value = 100)
      shiny::updateNumericInput(session, "msmooth_overlap", value = 50)
      shiny::updateNumericInput(session, "lower_detection_threshold", value = 0.08)
      shiny::updateNumericInput(session, "upper_detection_threshold", value = 0.2)
      shiny::updateNumericInput(session, "max_train_gap", value = 0.08)

    } else if (input$preset == "Tettigoniidae") {

      shiny::updateTextInput(session, "specimen_id", value = "")
      shiny::updateNumericInput(session, "msmooth_window", value = 900)
      shiny::updateNumericInput(session, "msmooth_overlap", value = 50)
      shiny::updateNumericInput(session, "upper_detection_threshold", value = 0.05)
      shiny::updateNumericInput(session, "lower_detection_threshold", value = 0.02)
      shiny::updateNumericInput(session, "max_train_gap", value = 0.1)

    }
  })

  output$audioPlot <- plotly::renderPlotly({
    shiny::req(result())
    temp_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(result()$plot, temp_file, selfcontained = TRUE)
    temp_file <<- temp_file

    p <- result()$plot
  })

  output$summary_data <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$summary_data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Summary"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })

  output$motif_data <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$motif_data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Motif Data"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })

  output$train_data <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$train_data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Train Data"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })

  output$params <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$params,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Parameters"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE,
                    paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })

  output$savePlot <- shiny::downloadHandler(
    filename = function() {
      paste0(input$specimen_id, "_tempstats_plot.html")
    },
    content = function(file) {
      shiny::req(temp_file)
      file.copy(temp_file, file)
    }
  )

  output$saveData <- shiny::downloadHandler(
    filename = function() {
      paste(input$specimen_id, "tempstats_data.xlsx", sep = "_")
    },
    content = function(file) {
      shiny::req(result())
      data_list <- list(
        "Summary" = result()$summary_data,
        "Motif Data" = result()$motif_data,
        "Train Data" = result()$train_data,
        "Parameters" = result()$params
      )
      writexl::write_xlsx(data_list, path = file)
    }
  )

  # Stop app when the tab is closed with the "X" button
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  # Stop app when the "Close app" button is used
  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

}
