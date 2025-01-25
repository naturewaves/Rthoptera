#' Oscillogram with ggplot2
#'
#' @param wave A Wave object
#' @param y_title Character. A title for the Y-axis.
#' @param x_title Character. A title for the X-axis.
#' @param save Logical. If TRUE, the plot is saved in the working directory.
#' Defaults to FALSE.
#' @param file_name Character. Name for the PNG file to be saved.
#' A suffix ("_oscillogram")
#' is added by default.
#' @param show_x Logical. If TRUE (default), the X-axis is plotted.
#'
#' @return An oscillogram plot. This plot is meant to be used with other
#' plots with matching the X-axis, therefore it lacks its labels.
#' @export
#'
#' @examples
#' \dontrun{
#' oscillgoram_ggplot(coryphoda)
#' }
oscillogram_ggplot <- function(wave,
                                y_title = "Relative Amplitude",
                                x_title = "",
                                save = TRUE,
                                file_name = "",
                                show_x = TRUE) {

  oscillo_df <- oscillo_df(wave)

  oscillo_plot <- ggplot(oscillo_df, aes(x = time, y = amplitude)) +
    geom_line(color = "black") +
    theme_minimal(base_size = 15) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(n.breaks = 3, expand = c(0.1, 0.1)) +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 10, unit = "pt"),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.line.y = element_line(colour = "black"),
      axis.line.x = if (show_x) element_line(colour = "black") else element_blank(),
      axis.ticks.x = if (show_x) element_line(colour = "black") else element_blank(),
      axis.title.x = if (show_x) element_text(size = 10) else element_blank(),
      axis.text.x = if (show_x) element_text(size = 10) else element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) +
    labs(y = y_title, x = x_title)

  if (save) {

    ggsave(paste(file_name, "oscillogram.png", sep = "_"),
           bg = "white", width = 2000, height = 800,
           units = "px", dpi = 300)

  }

  return(oscillo_plot)

}
