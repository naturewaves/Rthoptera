ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  titlePanel("Wave Timeline Editor"),

  tags$script(HTML(
    "
    document.addEventListener('DOMContentLoaded', function() {
      const timeline = document.getElementById('timeline');

      // Allow dropping on the timeline
      timeline.addEventListener('dragover', function(event) {
        event.preventDefault();
        timeline.style.backgroundColor = '#f0f0f0';  // Visual feedback for drop target
      });

      timeline.addEventListener('dragleave', function(event) {
        timeline.style.backgroundColor = '';  // Reset background color when leaving
      });

      timeline.addEventListener('drop', function(event) {
        event.preventDefault();
        timeline.style.backgroundColor = '';  // Reset background color after drop
        const wave_name = event.dataTransfer.getData('text/plain');
        Shiny.setInputValue('wave_drop', wave_name, {priority: 'event'});  // Force the event to trigger
      });
    });
    "
  )),

  sidebarLayout(
    sidebarPanel(
      h4("Available Wave Objects"),
      uiOutput("wave_list")  # List of available wave objects
    ),

    mainPanel(
      h4("Timeline"),
      uiOutput("timeline"),  # Timeline where waves are dropped (now in the main panel)
      br(),
      h4("Merged Wave Summary"),
      verbatimTextOutput("merged_wave_summary"),  # Summary of the merged wave
      br(),
      uiOutput("sampling_rate_ui"),  # UI for selecting the target sampling rate
      br(),
      uiOutput("normalize_checkbox_ui"),  # UI for the normalization checkbox
      br(),
      actionButton("merge_button", "Merge Waves"),  # Button to merge waves
      br(),
      textInput("new_wave_name", "Enter a name for the merged wave:"),
      actionButton("save_button", "Save Wave to R Memory"),  # Button to save the merged wave
      br(),
      downloadButton("download_merged_wave", "Download Merged Wave")  # Button to download the merged wave
    )
  )
)
