# Javascript code to customize the "close" button
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- function(request) {
  shiny::tagList(
    shiny::h1("Multi-Mean Spectra", style = "font-size: 28px; margin-left: 15px;"),
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
      shiny::tags$head(
        shiny::tags$style(
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

            .row {
              margin-bottom: 10px; /* Adds vertical space between rows */
            }

            /* Consistent button layout */
            .btn-space {
              margin-right: 5px;
              border: 2px solid green;
            }

            .btn-down {
              margin-right: 5px;
              border: 2px solid dodgerblue;
            }

            #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 5px; /* Button (inside) padding */
              border-radius: 5px; /* Optional: Rounded corners */
            }

            /* Adjust container for better fit */
            .container-fluid {
              max-width: 99%;
              max-height: 99%;
              padding-left: 5px;
              padding-right: 5px;
            }
            "
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(2,
                      shiny::div(style = "margin-right: 5px;",
                                 shiny::selectInput("wave_select", "Select a Wave Object:", choices = NULL, width = '100%')
                      )
        ),
        shiny::column(1,
                      shiny::div(style = "margin-right: 5px;",
                                 shiny::selectInput("wl", "Window Length: ", selected = 4096, choices = c(512, 1024, 2048, 4096, 8192), width = '90%')
                      )
        ),
        shiny::column(2,
                      shiny::verticalLayout(
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::textInput("plot_title", "Plot Title:", value = "", width = '100%')
                        ),
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::selectInput("selection_choice", "Selection Name:",
                                                      choices = c("closing", "opening", "male", "female", "Custom..."),
                                                      selected = "closing", width = '100%')
                        ),
                        shiny::conditionalPanel(
                          condition = "input.selection_choice == 'Custom...'",
                          shiny::div(style = "margin-right: 5px;",
                                     shiny::textInput("custom_selection_name", "Custom Name:", value = "", width = '100%')
                          )
                        )
                      )
        ),
        shiny::column(1,
                      shiny::verticalLayout(
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::actionButton("plot_button", "Plot", class = "btn-space")
                        ),
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::actionButton("add_selection", "Add Selection", class = "btn-space")
                        )
                      )
        ),
        shiny::column(1,
                      shiny::div(style = "margin-right: 5px;",
                                 shiny::numericInput("alpha", "Opacity", value = 0.9, min = 0.1, max = 1, step = 0.1), width = '60%'
                      )
        ),
        shiny::column(2,
                      shiny::div(style = "margin-right: 5px;",
                                 shiny::textInput("file_name", "File prefix:", value = "", width = '70%')
                      )
        ),
        shiny::column(2,
                      shiny::verticalLayout(
                        shiny::div(style = "margin-bottom: 5px;",
                                   shiny::downloadButton("download_oscillogram", "Download Oscillogram", class = "btn-down")
                        ),
                        shiny::div(style = "margin-bottom: 5px;",
                                   shiny::downloadButton("download_power_spectra", "Download Power Spectra", class = "btn-down")
                        ),
                        shiny::div(style = "margin-right: 5px;",
                                   shiny::actionButton("close", "Close App")
                        )
                      )
        )
      ),

      shiny::fluidRow(
        shiny::column(12,
                      shiny::plotOutput("oscillogram", height = "150px", width = "100%",
                                        brush = shiny::brushOpts(id = "wave_brush", direction = "x"))
        ),
        shiny::column(12,
                      shinycssloaders::withSpinner(plotly::plotlyOutput("mean_spectrum", height = "350px", width = "100%"))
        )
      )
    )
  )
}
