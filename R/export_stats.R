#' Export Data Frames and HTML Widgets from a List
#'
#' This function exports all data frames in a list to an Excel file (one sheet per data frame)
#' and any HTML widgets (e.g., plotly, leaflet) to a self-contained HTML file.
#'
#' @param list_name A named list containing data frames and/or HTML widget objects
#' @param output_name Base name for output files (without extension).
#'                   The function will create two files: `output_name.xlsx` and `output_name.html`.
#'
#' @return Invisible NULL. The function primarily produces files as side effects:
#'         - An Excel file containing all data frames (if any exist)
#'         - An HTML file containing the widget (if any exists)
#'         - Messages indicating which files were created
#'
#' @details
#' The function handles special characters in data frame names by removing them before creating Excel sheets.
#' HTML widgets are saved as self-contained files (no external dependencies) using `htmlwidgets::saveWidget()`.
#'
#' @examples
#' \dontrun{
#' # Create a sample list with data frames and a widget
#' my_list <- list(
#'   iris_data = iris,
#'   mtcars_data = mtcars,
#'   my_plot = plotly::plot_ly(x = 1:10, y = rnorm(10))
#'
#' # Export to files
#' export_stats(my_list, "my_export")
#'
#' # Results in:
#' # - "my_export.xlsx" with sheets "iris_data" and "mtcars_data"
#' # - "my_export.html" with the plotly visualization
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom htmlwidgets saveWidget
#' @export
export_stats <- function(list_name, output_name, path) {

  current_dir <- getwd()
  on.exit(setwd(current_dir))
  setwd(path)

  # Initialize workbook
  wb <- openxlsx::createWorkbook()
  df_found <- FALSE

  # Process each element
  for (elem_name in names(list_name)) {
    element <- list_name[[elem_name]]

    # Handle data frames
    if (is.data.frame(element)) {
      df_found <- TRUE
      sheet_name <- substr(gsub("[\\[\\]\\*\\?\\/\\\\]", "", elem_name), 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, element)
    }

   }

  # Save Excel if data frames found
  if (df_found) {
    xlsx_filename <- paste0(output_name, ".xlsx")
    openxlsx::saveWorkbook(wb, xlsx_filename, overwrite = TRUE)
    message("Data frames saved to: ", xlsx_filename)
  } else {
    message("No data frames found in the list.")
  }


}

