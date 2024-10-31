server <- function(input, output, session) {
  plotVisible <- shiny::reactiveVal(FALSE)
  savedPlot <- shiny::reactiveVal(NULL)
  savedImage <- shiny::reactiveVal(NULL)

  observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
      inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObject", choices = waveObjects)
  })

  # Calculate the heights for the patchwork
  heights <- shiny::reactive({
    value2 <- (input$osc.height / 100) * 10
    value1 <- 10 - value2
    c(value1, value2)
  })

  shiny::observeEvent(input$multiplot, {
    shiny::req(input$waveObject)
    shiny::req(input$noise.cutoff)

    wave <- tryCatch({
      get(input$waveObject, envir = .GlobalEnv)
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        paste("Could not find the selected wave object:", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return(NULL)
    })

    plotParams <- list(
      wave = wave,
      cutoff = shiny::isolate(input$noise.cutoff),
      scale.type = shiny::isolate(input$meanspecScale),
      heights = heights()
    )

    plotVisible(TRUE)

    output$specPlotOutput <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput("specPlot", height = "auto", width = "auto"), type = 1)
    })

    output$specPlot <- shiny::renderPlot({
      tryCatch({
        combined_plot <- multiplot_ggplot(
          wave = wave,
          cutoff = shiny::isolate(input$noise.cutoff),
          scale_type = shiny::isolate(input$meanspecScale),
          heights = heights()
        )
        print(combined_plot)
        savedPlot(combined_plot)

        temp_file <- tempfile(fileext = ".png")
        bg <- ifelse(input$transparentBg, "transparent", "white")
        ggplot2::ggsave(temp_file,
                        plot = combined_plot,
                        width = shiny::isolate(input$imgWidth), height = shiny::isolate(input$imgHeight),
                        units = "in", dpi = 300, bg = bg
        )
        savedImage(temp_file)

      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        NULL
      })
    }, height = function() {
      shiny::isolate(input$imgHeight) * 100
    }, width = function() {
      shiny::isolate(input$imgWidth) * 100
    })
  })

  output$saveImage <- shiny::downloadHandler(
    filename = function() {
      paste(as.character(input$waveObject), "_multiplot.png", sep = "")
    },
    content = function(file) {
      shiny::req(savedImage())
      file.copy(savedImage(), file)
    }
  )

  shiny::observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    shiny::stopApp()
  })

  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}
