#' Launch a Shiny App from the package
#'
#' @param app_name The (partial or full) name of the Shiny app. If NULL,
#' lists available apps.
#' @return Launches the Shiny app or lists available apps if no name is provided
#' @export
launch_app <- function(app_name = NULL) {

  app_dir_root <- system.file("shiny", package = "Rthoptera")

  # Get the list of available apps
  available_apps <- list.dirs(app_dir_root, full.names = FALSE,
                              recursive = FALSE)

  # If app_name is NULL, list available apps
  if (is.null(app_name)) {
    if (length(available_apps) == 0) {
      stop("No Shiny apps found in the package.")
    }
    message("Available apps:")
    for (app in available_apps) {
      message(" - ", app)
    }
    return(invisible(available_apps))
  }

  # Perform partial matching of app_name
  matches <- available_apps[grepl(app_name, available_apps, ignore.case = TRUE)]

  if (length(matches) == 0) {
    stop("No matching app found for '", app_name, "'. Available apps are:\n - ",
         paste(available_apps, collapse = "\n - "))
  } else if (length(matches) > 1) {
    stop("Multiple apps match '", app_name,
         "'. Please specify more precisely. Matching apps:\n - ",
         paste(matches, collapse = "\n - "))
  } else {
    # One match found
    app_name <- matches
  }

  # Construct the app directory path
  app_dir <- file.path(app_dir_root, app_name)

  # Launch the app
  shiny::runApp(app_dir, display.mode = "normal")
}
