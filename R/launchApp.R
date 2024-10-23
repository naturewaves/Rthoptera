#' Launch a Shiny App from the package
#'
#' @param app_name The name of the Shiny app folder inside inst/shiny. If NULL, lists available apps.
#' @return Launches the Shiny app or lists available apps if no name is provided
#' @export
launchApp <- function(app_name = NULL) {

  appDirRoot <- system.file("shiny", package = "Rthoptera")

  # If app_name is NULL, list available apps
  if (is.null(app_name)) {
    available_apps <- list.dirs(appDirRoot, full.names = FALSE, recursive = FALSE)

    if (length(available_apps) == 0) {
      stop("No Shiny apps found in the package.")
    }

    message("Available apps: ", paste(available_apps, collapse = ", "))
    return(invisible(available_apps))
  }

  # Construct the app directory path
  appDir <- file.path(appDirRoot, app_name)

  # Check if the app directory exists
  if (!file.exists(appDir)) {
    stop("Could not find app directory. Make sure the app exists in inst/shiny/")
  }

  # Launch the app
  shiny::runApp(appDir, display.mode = "normal")
}
