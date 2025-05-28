ui <- function(request){
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$style(
      HTML("
        /* General body styling */
        body {
          background-color: #252626;
          color: #ffffff;
          margin: 5px;
        }

    /* Styling for all inputs */
    .form-control, .shiny-input-container input, .shiny-input-container select, 
    .shiny-input-container textarea, .selectize-input, .selectize-dropdown {
      background-color: #495057 !important;
      border: 1px solid #6c757d !important;
      color: #ffffff !important;
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
          margin-right: 10px; /* Increases horizontal space between inputs */
        }
        .js-irs-0 .irs-grid,
        .js-irs-0 .irs-min,
        .js-irs-0 .irs-max,
        .js-irs-0 .irs-single,
        .js-irs-0 .irs-grid-pol{
          display: none !important; /* Hides slider labels and ticks */
        }
        .slider-container .shiny-input-container {
          width: 100% !important; /* Ensure slider takes full width */
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


        #downloadPlot {
          border: 2px solid dodgerblue; /* Blue contour */
          padding: 5px 5px; /* Button (inside) padding */
          border-radius: 5px; /* Optional: Rounded corners */
        }
        #close {
          border: 2px solid red; /* Red contour */
          padding: 5px 5px; /* Button (inside) padding */
          border-radius: 5px; /* Optional: Rounded corners */
          margin-left: 10px;
        }
        
        /* Custom styling for plot output */
        .shiny-plot-output {
          background-color: #252626 !important;
          border-radius: 5px;
          padding: 10px;
        }
        
        /* Panel styling */
        .well {
          background-color: #343a40;
          border-color: #6c757d;
        }
        
        /* Select input dropdown */
        .selectize-dropdown {
          background-color: #495057;
          color: #ffffff;
        }
        
        .selectize-input {
          background-color: #495057;
          color: #ffffff;
        }
        
        .selectize-dropdown-content .option {
          color: #ffffff;
        }
        
        /* Checkbox styling */
        .checkbox {
          color: #ffffff;
        }
        
        /* File input styling */
        .shiny-input-container .progress {
          background-color: #495057;
        }
        
        /* Spinner color */
        .shinycssloaders .loading-spinner {
          color: #ffffff !important;
        }
      ")
    )),
    shiny::h1("Spectrum with Image Composite", style = "font-size: 28px; margin-left: 15px;"),
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("waveObject", "Select a wave object:", choices = NULL),
          shiny::selectInput("wl", "Window Length:", selected = 1024,
                             choices = c(128, 256, 512, 1024, 2048, 4096, 8192)),
          shiny::numericInput("ovlp", "Overlap (%):", value = 75, min = 0, max = 100),
          shiny::selectInput("wn", "Window Function:",
                             choices = c("bartlett", "blackman", "flattop",
                                         "hamming", "hanning", "rectangle"),
                             selected = "hanning"),
          shiny::selectInput("scale", "Scale:", choices = c("linear", "dB", "dBFS")),
          shiny::conditionalPanel(
            condition = "input.scale == 'dB' || input.scale == 'dBFS'",
            shiny::numericInput("yMin", "Minimum amplitude (dB):", value = -50),
            shiny::textInput("yBreaks", "Amplitude Ticks:", value = "-40, -20, 0")
          ),
          shiny::fileInput("image_file", "Select Image:", accept = c("image/png", "image/jpeg", "image/tiff")),
          shiny::selectInput("label_color_A", "Label A Color:", 
                             choices = c("black", "white"), selected = "black"),
          shiny::selectInput("label_color_B", "Label B Color:", 
                             choices = c("black", "white"), selected = "black"),
          # shiny::checkboxInput("show_composite", "Show Composite", value = TRUE),
          width = 2
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(1, shiny::verticalLayout(
              shiny::numericInput("fmin", "Min. Freq:",
                                  value = 0, min =0, max=100, step = 1),
              shiny::numericInput("fmax", "Max. Freq:",
                                  value = 48, min =10, max=193, step = 1)
            )),
            shiny::column(2, shiny::verticalLayout(
              shiny::numericInput("x.breaks", "Nr of Frequency Ticks:",
                                  value = 20, min = 4, max = 193, step = 1),
              shiny::textInput("fill", "Fill Color:", value = "black")
            )),
            shiny::column(2, 
                          shiny::selectInput("fun", "Function:", choices = c("mean", "median", "var", "sd"))
            ),
            shiny::column(1, shiny::actionButton("plot", "Plot")),
            shiny::column(1, shiny::downloadButton("downloadPlot", "Download")),
            shiny::column(1, shiny::actionButton("close", "Close App"))
          ),
          
          shiny::fluidRow(
            shinycssloaders::withSpinner(shiny::plotOutput("plot"))
          ),
          width = 10
        )
      )
    )
  )
}