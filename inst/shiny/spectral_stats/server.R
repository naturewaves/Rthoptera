server <- function(input, output, session) {

  # Store reactive values
  values <- shiny::reactiveValues(speciesName = "", callType = "")

  # Update the title whenever the input changes
  shiny::observe({
    values$speciesName <- input$specimen.id
    values$callType <- input$sound.type
  })

  # Observer to update available wave objects in the environment
  shiny::observe({
    wave_names <- ls(envir = .GlobalEnv)
    wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "selectedWave", choices = wave_names)
  })

  shiny::observeEvent(input$scale, {
    if (input$scale == "db") {
      shiny::updateNumericInput(session, "cutoff", value = -6, min = -100, max = -3, step = 1)
    } else {
      shiny::updateNumericInput(session, "cutoff", value = 0.1, min = 0.05, max = 0.95, step = 0.05)
    }
  })

  # Reactive expression for re-running analysis
  result <- shiny::eventReactive(input$run, {
    shiny::req(input$selectedWave)
    wave <- get(input$selectedWave, envir = .GlobalEnv)
    spectral_stats(
      wave = wave,
      specimen_id = input$specimen.id,
      sound_type = input$sound.type,
      temp = input$temp,
      hpf = input$hpf,
      cutoff = as.numeric(input$cutoff),
      total_range = input$total,
      lines = TRUE,
      db = (input$scale == "db"),
      robust = input$robust
    )
  })

  output$plotOutput <- shiny::renderUI({
    shiny::req(result())
    plotly::plotlyOutput("plotlyPlot", width = '1150px')
  })

  output$plotlyPlot <- plotly::renderPlotly({
    shiny::req(result())
    result()$plot |>
      plotly::layout(
        margin = list(l = 80, r = 0, t = 80, b = 80),
        annotations = list(
          list(
            text = input$sound.type,
            font = list(size = 13, color = 'black'),
            showarrow = FALSE, align = 'right',
            x = 0, y = 1.1, xref = 'x', yref = 'paper'
          )
        )
      )
  })

  output$dataOutput <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$data,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: center;",
                    class = "caption-top",
                    "Spectral Data"
                  ),
                  options = list(
                    pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                    columnDefs = list(list(orderable = FALSE, targets = "_all"))
                  )
    )
  })

  # Download data
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_stats.csv")
    },
    content = function(file) {
      utils::write.csv(result()$data, file, row.names = FALSE)
    }
  )

  # Save data frame in R environment
  shiny::observeEvent(input$saveDataEnv, {
    shiny::req(result(), input$dataName)
    assign(input$dataName, result()$data, envir = .GlobalEnv)
    shiny::showModal(shiny::modalDialog(
      title = "Saved",
      paste0("Available as '", input$dataName, "' in the R environment."),
      easyClose = TRUE,
      footer = shiny::modalButton("OK")
    ))
  })

  # Save plot
  output$savePlot <- shiny::downloadHandler(
    filename = function() {
      paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_analysis.html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(result()$plot, file, selfcontained = TRUE)
    }
  )

  # Stop app on session end
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  # Stop app with "Close App" button
  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })
}


