server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  # Spectrum plot function (unchanged)
  spectrum_plot2 <- function(wave, wl = 4096, ovlp = 75, scale = "linear",
                             y.min = -100, y.breaks = c(-40, -20, 0), x.breaks = 6,
                             y.position = "left", x.position = "bottom", flip = TRUE,
                             fill = 'black', fun = "mean", wn = "blackman",
                             show.x.title = TRUE, show.y.title = TRUE, add.params = TRUE,
                             plot.title = NULL, italic_title = FALSE, fmin = NULL, fmax = NULL) {
    
    if (is.null(fmax) || fmax == 0) {
      fmax <- wave@samp.rate / 2 / 1000
    }
    
    fmin <- as.numeric(fmin)
    fmax <- as.numeric(fmax)
    
    if (scale == "dBFS") {
      meanspec_data <- seewave::meanspec(wave, f = wave@samp.rate, PSD = TRUE,
                                         wl = wl, ovlp = ovlp,
                                         plot = FALSE, norm = FALSE, wn = wn, FUN = fun,
                                         flim = c(fmin, fmax))
      amp_max <- if (wave@bit == 16) 32768 else if (wave@bit == 24) 8388607 else if (wave@bit == 32) 2147483647 else stop("Unsupported bit depth")
      meanspec_data[, 2] <- 20 * log10(abs(meanspec_data[, 2]) / amp_max)
    } else {
      meanspec_data <- seewave::meanspec(wave, f = wave@samp.rate, wl = wl, ovlp = ovlp,
                                         plot = FALSE, norm = TRUE, 
                                         dB = if (scale == "dB") "max0" else NULL,
                                         wn = wn, FUN = fun, flim = c(fmin, fmax))
    }
    
    meanspec_data <- data.frame(
      freq = meanspec_data[, 1],
      mean_amp = meanspec_data[, 2]
    )
    
    plot <- meanspec_data %>%
      ggplot2::ggplot(ggplot2::aes(x = freq, y = mean_amp)) +
      ggplot2::theme_minimal(base_size = 15)
    
    if (scale == "dB" || scale == "dBFS") {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(x = freq, ymin = y.min, ymax = mean_amp), fill = fill) +
        ggplot2::scale_y_continuous(breaks = y.breaks, limits = c(y.min, 0), 
                                    expand = ggplot2::expansion(mult = c(0, .1)), 
                                    position = y.position)
    } else {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(x = freq, ymin = 0, ymax = mean_amp), fill = fill) +
        ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), 
                                    expand = ggplot2::expansion(mult = c(0, .1)), 
                                    position = y.position,
                                    labels = function(x) ifelse(x == 0 | x == 1, 
                                                                as.character(as.integer(x)), 
                                                                as.character(x)))
    }
    
    title_style <- if (italic_title) "italic" else "plain"
    
    plot <- plot +
      ggplot2::scale_x_continuous(limits = c(fmin, fmax), expand = c(0, 0), 
                                  position = x.position, 
                                  breaks = scales::breaks_pretty(n = x.breaks), 
                                  labels = scales::label_number(zero.print = "")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        axis.ticks = ggplot2::element_line(colour = "black"),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "none",
        plot.title = ggplot2::element_text(face = title_style)
      ) +
      ggplot2::labs(
        x = if (show.x.title) "Frequency (kHz)" else NULL,
        y = if (show.y.title) "Amplitude" else NULL,
        title = plot.title
      )
    
    if (add.params) {
      params_text <- paste0("win. function: ", wn, "\nwin. size: ", wl,
                            "\noverlap:", ovlp, "%", "\nfunction: ", fun,
                            "\nscale: ", scale)
      
      plot <- plot +
        ggplot2::annotate("text", x = Inf, y = Inf, label = params_text, 
                          hjust = 1.1, vjust = 1.1, size = 4, color = "black")
    }
    
    if (flip) {
      plot <- plot + ggplot2::coord_flip()
    }
    
    return(plot)
  }
  
  # Wave object observer
  shiny::observe({
    waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), 
                                                 function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
    shiny::updateSelectInput(session, "waveObject", choices = waveObjects)
  })
  
  shiny::observeEvent(input$waveObject, {
    shiny::req(input$waveObject)
    wave <- get(input$waveObject, envir = .GlobalEnv)
    shiny::updateNumericInput(session, "fmax", value = wave@samp.rate / 2 / 1000)
  })
  
  # Image processing system
  cropped_image <- reactiveVal(NULL)
  
  observeEvent(input$image_file, {
    req(input$image_file)
    
    # Read the image
    img <- magick::image_read(input$image_file$datapath)
    img_info <- magick::image_info(img)
    
    # Target dimensions (in pixels at 100dpi)
    target_width_px <- 9.37 * 100
    target_height_px <- 7.5 * 100
    
    # Calculate scaling factor based on height
    scale_factor <- target_height_px / img_info$height
    
    # Scale the image to match the target height
    scaled_img <- magick::image_scale(img, paste0("x", target_height_px))
    
    # Get dimensions after scaling
    scaled_info <- magick::image_info(scaled_img)
    
    # Calculate crop area (anchor bottom right)
    if (scaled_info$width > target_width_px) {
      # If scaled width exceeds target, crop from the left
      x_offset <- scaled_info$width - target_width_px
      cropped <- magick::image_crop(scaled_img, 
                                    geometry = paste0(target_width_px, "x", target_height_px, "+", x_offset, "+0"))
    } else {
      # If scaled width is less than target, just use the scaled image (it will be centered)
      cropped <- scaled_img
    }
    
    cropped_image(cropped)
  })
  
  # Composite plot
  composite_plot <- shiny::reactive({
    shiny::req(plotData(), cropped_image())
    
    # Fixed dimensions
    spec_width <- 3.98   # inches
    img_display_width <- 9.37  # inches
    height <- 7.5        # inches
    total_width <- spec_width + img_display_width
    
    # Get spectrum plot
    spec_plot <- plotData()$plot
    spec_grob <- ggplot2::ggplotGrob(spec_plot)
    
    # Create main viewport
    grid::grid.newpage()
    main_vp <- viewport(
      width = unit(total_width, "inches"),
      height = unit(height, "inches"),
      clip = "on"
    )
    pushViewport(main_vp)
    # Draw the cropped image
    grid::grid.raster(
      as.raster(cropped_image()),
      width = unit(img_display_width, "inches"),
      height = unit(height, "inches"),
      x = unit(spec_width, "inches"),
      just = "left"
    )
    
    # Draw the spectrum plot
    pushViewport(viewport(
      x = unit(0, "npc"),
      width = unit(spec_width, "inches"),
      just = "left",
      clip = "on"
    ))
    grid::grid.draw(spec_grob)
    
    # Add label A
    grid::grid.text("A",
                    x = unit(0.88, "npc"), y = unit(0.93, "npc"),
                    gp = gpar(fontface = "bold", col = input$label_color_A, cex = 3))
    popViewport()
    
    # Add label B
    pushViewport(viewport(
      x = unit(spec_width, "inches"),
      width = unit(img_display_width, "inches"),
      just = "left",
      clip = "on"
    ))
    grid::grid.text("B",
                    x = unit(0.95, "npc"), y = unit(0.93, "npc"),
                    gp = gpar(fontface = "bold", col = input$label_color_B, cex = 3))
    grid::grid.rect(
      gp = gpar(col = "black", fill = NA, lwd = 8)  
    )
    popViewport()
    pushViewport(main_vp)
    grid::grid.rect(
      gp = gpar(col = "black", fill = NA, lwd = 8)  
    )
    popViewport()
    grid::grid.grab()
  })
  
  # Plot data
  plotData <- shiny::eventReactive(input$plot, {
    shiny::req(input$waveObject)
    wave <- get(input$waveObject, envir = .GlobalEnv)
    
    fmax_value <- if (is.null(input$fmax) || input$fmax == "") {
      wave@samp.rate / 2 / 1000
    } else {
      input$fmax
    }
    
    yBreaks <- as.numeric(unlist(strsplit(isolate(input$yBreaks), ",")))
    
    list(
      plot = spectrum_plot2(wave,
                            wl = isolate(as.numeric(input$wl)),
                            plot.title = NULL,
                            italic_title = TRUE,
                            ovlp = isolate(input$ovlp),
                            scale = isolate(input$scale),
                            y.min = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") isolate(input$yMin) else NULL,
                            y.breaks = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") yBreaks else NULL,
                            x.breaks = isolate(input$x.breaks),
                            y.position = "left",
                            x.position = "bottom",
                            flip = TRUE,
                            fill = isolate(input$fill),
                            fun = isolate(input$fun),
                            wn = isolate(input$wn),
                            show.x.title = TRUE,
                            show.y.title = TRUE,
                            add.params = FALSE,
                            fmin = input$fmin,
                            fmax = fmax_value),
      width = 3.98,
      height = 7.5
    )
  })
  
  # Render plot
  output$plot <- shiny::renderPlot({
    if (!is.null(cropped_image())) {
      print(composite_plot())
    } else {
      shiny::req(plotData())
      print(plotData()$plot)
    }
  },
  width = function() {
    if (!is.null(cropped_image())) {
      return((3.98 + 9.37) * 72)
    } else {
      return(3.98 * 72)
    }
  },
  height = function() {
    return(7.5 * 72)
  })
  
  # Download handler
  savedImage <- shiny::reactiveVal(NULL)
  
  observeEvent(input$plot, {
    if (!is.null(cropped_image())) {
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = (3.98 + 9.37) * 300, height = 7.5 * 300, res = 300)
      grid::grid.draw(composite_plot())
      dev.off()
      savedImage(temp_file)
    }
  })
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste(input$waveObject, "_psd_habitus_", input$fun,".png",sep = "")
    },
    content = function(file) {
      shiny::req(savedImage())
      file.copy(savedImage(), file)
    }
  )
  
  # Close app
  shiny::observeEvent(input$close, {
    shiny::stopApp()
  })
}