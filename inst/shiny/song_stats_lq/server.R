server <- function(input, output, session) {

  # Observer to update available wave objects in the environment
  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "selectedWave", choices = wave_names)
  })

  result <- shiny::eventReactive(input$run, {
    shiny::req(input$selectedWave)
    wave <- get(input$selectedWave, envir = .GlobalEnv)
    song_stats_lq(wave,
                  specimen_id = input$specimen_id,
                  ssmooth = as.numeric(shiny::isolate(input$ssmooth)),
                  peakfinder_ws = input$peakfinder_ws,
                  peakfinder_threshold = input$peakfinder_threshold,
                  max_peak_gap = input$max_peak_gap,
                  max_train_gap = input$max_train_gap,
                  motif_seq = input$motif_seq,
                  max_motif_gap = input$max_motif_gap,
                  detection_threshold = input$detection_threshold,
                  norm_env = input$norm,
                  db_threshold = 20)
  })

  shiny::observeEvent(input$preset, {

    if (input$preset == "Gryllidae") {
      shiny::updateNumericInput(session, "ssmooth", value = 0)
      shiny::updateNumericInput(session, "peakfinder_ws", value = 5)
      shiny::updateNumericInput(session, "peakfinder_threshold", value = 0.05)
      shiny::updateNumericInput(session, "max_peak_gap", value = 0.01)
      shiny::updateNumericInput(session, "max_train_gap", value = 0.3)
      shiny::updateNumericInput(session, "detection_threshold", value = 0.15)

    } else if (input$preset == "Tettigoniidae") {
      shiny::updateNumericInput(session, "ssmooth", value = 100)
      shiny::updateNumericInput(session, "peakfinder_ws", value = 50)
      shiny::updateNumericInput(session, "peakfinder_threshold", value = 0.01)
      shiny::updateNumericInput(session, "max_peak_gap", value = 0.01)
      shiny::updateNumericInput(session, "max_train_gap", value = 0.3)
      shiny::updateNumericInput(session, "detection_threshold", value = 0.15)
    }
  })

  output$audioPlot <- plotly::renderPlotly({
    shiny::req(result())

    tryCatch({
      plot_obj <- result()$plot

      if (inherits(plot_obj, "plotly")) {
        plot_obj %>%
          plotly::layout(title = input$specimen_id,
                         margin = list(l = 80, r = 0, t = 80, b = 80))

        temp_file <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plot_obj, temp_file, selfcontained = TRUE)
        temp_file <<- temp_file

        return(plot_obj)
      } else {
        shiny::showNotification("The plot object is invalid or missing.", type = "error")
        return(NULL)
      }

    }, error = function(e) {
      shiny::showNotification(paste("An error occurred:", e$message), type = "error")
      return(NULL)
    })
  })

  output$summary_data <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$summary_data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Summary Data"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })


  output$motif_seq_data <- DT::renderDT({
    if (isTRUE(input$motif_seq)) {
      shiny::req(result())
      DT::datatable(
        result()$motif_seq_data,
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left;",
          class = "caption-top",
          "Motif Sequence Data"
        ),
        options = list(
          pageLength = 1,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE,
          info = FALSE,
          columnDefs = list(list(orderable = FALSE, targets = "_all"))
        )
      )
    } else {
      NULL
    }
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

  output$peak_data <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$peak_data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left;",
                    class = "caption-top",
                    "Peak Data"
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
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  ))
  })

  output$saveData <- shiny::downloadHandler(
    filename = function() {
      paste(input$specimen_id, "tempstats_lq_data.xlsx", sep = "_")
    },
    content = function(file) {
      shiny::req(result())

      data_list <- list(
        "Summary Data" = result()$summary_data
        )

      if (isTRUE(input$motif_seq)) {
        data_list[["Motif Sequence Data"]] <- result()$motif_seq_data
      }

      data_list <- c(data_list, list(
          "Motif Data" = result()$motif_data,
          "Train Data" = result()$train_data,
          "Peak Data" = result()$peak_data,
          "Parameters" = result()$params
        ))

      writexl::write_xlsx(data_list, path = file)
    }
  )

  output$savePlot <- shiny::downloadHandler(
    filename = function() {
      paste(input$specimen_id, "tempstats_hq_plot.html", sep = "_")
    },
    content = function(file) {
      shiny::req(temp_file)
      file.copy(temp_file, file)
    }
  )

  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

}
