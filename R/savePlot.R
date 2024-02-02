#' Save plot
#'
#' @param plot Plot created by either plotFreq or plotKM. Must be a "ggplot" or "ggsurvfit" object
#' @param width Width of figure, in inches. Decreasing width/height increases relative size of text. Defaults to 10 for ggplots and 8 for ggsurvfit
#' @param height Height of figure, in inches. Decreasing width/height increases relative size of text. Defaults 8 for ggplots and 4 for ggsurvfit
#' @param separateLegend Whether or not to save legend separately to a different file. Defaults to False
#'
#' @return Saves the plot in "png" format and in standardized dimensions and resolution. Returns the path of the saved figure
#' @export
#'
savePlot <- function(plot, width = NULL, height = NULL, separateLegend = F) {
  if(inherits(plot, 'ggsurvfit')) {
    if(is.null(width)) width <- 8
    if(is.null(height)) height <- 4
  } else if(inherits(plot, 'ggplot')) {
    if(is.null(width)) width <- 10
    if(is.null(height)) height <- 8
  } else stop('"plot" must be a "ggplot" or "ggsurvfit" object')

  if(separateLegend) {
    plot.legend <- ggpubr::as_ggplot(ggpubr::get_legend(plot))
    plot <- plot + ggplot2::theme(legend.position = 'none')
  }

  show(plot)

  if(select.list(c('Yes','No'), title = '\nSave figure?')=='Yes') {
    save_directory <- rstudioapi::selectDirectory()
    filename <- paste0(readline('Figure name: '),'.png')

    if(separateLegend) {
      filename <- gsub('.png','_nolegend.png', filename)
      ggplot2::ggsave(filename = file.path(save_directory, paste0('Legend_',filename,'.png')),
                      plot = plot.legend,
                      device = 'png',
                      width = 16,
                      height = 6)
    }

    ggplot2::ggsave(filename = file.path(save_directory, filename),
                    plot = plot,
                    device = 'png',
                    width = width, height = height, units = 'in',
                    dpi=600)
    cat('Figure was saved to:\n  ', save_directory)
  }
}

#' Edit plot axes
#'
#' @param plot A ggplot object
#' @param x.toggleTitle Toggle whether or not the x-axis title appears
#' @param x.toggleLabels Toggle whether or not the x-axis labels appear
#' @param x.changeTitle Change the x-axis title and font size
#' @param x.changeLabels Change the x-axis labels and font size
#' @param y.toggleTitle Toggle whether or not the y-axis title appears
#' @param y.toggleLabels Toggle whether or not the y-axis labels appear
#' @param y.changeTitle Change the y-axis title and font size
#' @param y.changeLabels Change the y-axis labels and font size
#'
#' @return New plot with edited axes
#' @export
#'
editAxes <- function(plot,
                     x.toggleTitle = F, x.toggleLabels = F,
                     x.changeTitle = F, x.changeLabels = F,
                     y.toggleTitle = F, y.toggleLabels = F,
                     y.changeTitle = F, y.changeLabels = F) {
  if(!('ggplot' %in% class(plot))) stop('"plot" must be a "ggplot" object')

  edits <- list(x.toggleTitle = x.toggleTitle,
                x.toggleLabels = x.toggleLabels,
                x.changeTitle = x.changeTitle,
                x.changeLabels = x.changeLabels,
                y.toggleTitle = y.toggleTitle,
                y.toggleLabels = y.toggleLabels,
                y.changeTitle = y.changeTitle,
                y.changeLabels = y.changeLabels)

  show(plot)

  # If user did not specify any changes to make
  if(!any(as.logical(edits))) {
    edit.choices <- list(x.toggleTitle = 'Toggle x-axis title',
                         x.toggleLabels = 'Toggle x-axis labels',
                         x.changeTitle = 'Change x-axis title',
                         x.changeLabels = 'Change x-axis labels',
                         y.toggleTitle = 'Toggle y-axis title',
                         y.toggleLabels = 'Toggle y-axis labels',
                         y.changeTitle = 'Change y-axis title',
                         y.changeLabels = 'Change y-axis labels')

    edits[names(edits) %in% names(select.list(edit.choices,
                                              title = 'Which of the following changes would you like to make?',
                                              multiple = T))] <- T
  }

  plot.data <- ggplot2::ggplot_build(plot)

  # Change x-axis
  if(edits$x.toggleTitle) {
    if(length(plot$theme$axis.title.x)) plot <- plot + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    else plot <- plot + ggplot2::theme(axis.title.x = ggplot2::element_text())
    show(plot)
  }

  if(edits$x.toggleLabels) {
    if(length(plot$theme$axis.text.x)) plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    else plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text())
    show(plot)
  }

  if(edits$x.changeTitle) {
    plot <- plot + ggplot2::labs(x = readline('New x-axis title: '))
    show(plot)

    while(select.list(c('Yes','No'), title = '\nChange x-axis title font size?')=='Yes') {
      # Get the specified x-axis title font size if it exists, otherwise the general axis title font size
      x.title.size <- c(plot$theme$axis.title.x$size, plot$theme$axis.title$size, plot$theme$text$size)[1]
      cat('\nCurrent x-axis font size is', x.title.size)
      plot <- plot+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = readline('New font size: ')))
      show(plot)
    }
  }

  if(edits$x.changeLabels) {
    plot <- plot+
      ggplot2::scale_x_discrete(labels = lapply(plot.data$layout$panel_params[[1]]$x$get_labels(),
                                                function(x) readline(paste0('New label to replace "', x, '": '))))
    show(plot)

    while(select.list(c('Yes','No'), title = '\nChange x-axis labels font size?')=='Yes') {
      # Get the specified x-axis title font size if it exists, otherwise the general axis title font size
      x.labels.size <- c(plot$theme$axis.text.x$size, plot$theme$axis.text$size)[1]
      cat('\nCurrent x-axis font size is', x.labels.size)
      plot <- plot+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = readline('New font size: ')))
      show(plot)
    }
  }

  # Change y-axis
  if(edits$y.toggleTitle) {
    if(length(plot$theme$axis.title.y)) plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    else plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_text())
    show(plot)
  }

  if(edits$y.toggleLabels) {
    if(length(plot$theme$axis.text.y)) plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_blank())
    else plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_text())
    show(plot)
  }

  if(edits$y.changeTitle) {
    plot <- plot + ggplot2::labs(y = readline('New y-axis title: '))
    show(plot)

    while(select.list(c('Yes','No'), title = '\nChange y-axis title font size?')=='Yes') {
      # Get the specified x-axis title font size if it exists, otherwise the general axis title font size
      y.title.size <- c(plot$theme$axis.title.y$size, plot$theme$axis.title$size)[1]
      cat('\nCurrent y-axis font size is', y.title.size)
      plot <- plot+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = readline('New font size: ')))
      show(plot)
    }
  }

  if(edits$y.changeLabels) {
    plot <- plot+
      ggplot2::scale_y_discrete(labels = lapply(plot.data$layout$panel_params[[1]]$y$get_labels(),
                                                function(y) readline(paste0('New label to replace "', y, '": '))))
    show(plot)

    while(select.list(c('Yes','No'), title = '\nChange y-axis labels font size?')=='Yes') {
      # Get the specified y-axis title font size if it exists, otherwise the general axis title font size
      y.labels.size <- c(plot$theme$axis.text.y$size, plot$theme$axis.text$size)[1]
      cat('\nCurrent x-axis font size is', y.labels.size)
      plot <- plot+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = readline('New font size: ')))
      show(plot)
    }
  }

  show(plot)

  if(select.list(title = '\nSave figure?', choices = c('Yes', 'No')) == 'Yes') savePlot(plot)

  return(plot)
}

#' Edit plot legend
#'
#' @param plot A ggplot object
#' @param toggleTitle a
#' @param cycleColors a
#' @param changeTitle a
#' @param changeLabels a
#'
#' @return Plot with edited legend
#' @export
#'
changeLegend <- function(plot,
                         toggleTitle = F, cycleColors = F,
                         changeTitle = F, changeLabels = F) {
  if(!('ggplot' %in% class(plot))) stop('"plot" must be a "ggplot" object')

  edits <- list(toggleTitle = toggleTitle,
                cycleColors = cycleColors,
                changeTitle = changeTitle,
                changeLabels = changeLabels)

  show(plot)

  if(!any(as.logical(edits))) {
    edit.choices <- list(toggleTitle = 'Toggle legend title',
                         cycleColors = 'Cycle/rotate colors',
                         changeTitle = 'Change legend title',
                         changeLabels = 'Change legend labels')

    edits[names(edits) %in% names(select.list(edit.choices,
                                              title = 'Which of the following changes would you like to make?',
                                              multiple = T))] <- T
  }

  if(edits$toggleTitle) {
    if(length(plot$theme$legend.title)) plot <- plot + ggplot2::theme(legend.title = ggplot2::element_blank())
    else plot <- plot + ggplot2::theme(legend.title = ggplot2::element_text())
    show(plot)
  }

  if(edits$changeTitle) {
    plot <- plot + ggplot2::labs(x = readline('New legend title: '))
    show(plot)

    while(select.list(c('Yes','No'), title = '\nChange legend title font size?')=='Yes') {
      # Get the specified legend title font size if it exists, otherwise the general text size
      title.size <- c(plot$theme$legend.text$size, plot$theme$text$size)[1]
      cat('\nCurrent x-axis font size is', x.title.size)
      plot <- plot+
        ggplot2::theme(legend.title = ggplot2::element_text(size = readline('New font size: ')))
      show(plot)
    }
  }

  # colors.current <- unique(plot.data$data[[1]]$fill)
  # labels.current <- plot$scales$scales[unlist(lapply(plot$scales$scales,
  #                                                    function(n) any(grepl('fill', n$aesthetics))))][[1]]$labels

  # This is terrible code. But I cannot figure out how to extract color assignments for different legend groups/labels
  while(edits$cycleColors) {
    groups.orig <- getGroups(plot)
    colors.new <- c(unique(groups.orig$colors)[-1], unique(groups.orig$colors)[1]) ###
    plot <- plot + ggplot2::scale_fill_manual(values = colors.new,
                                              labels = groups.orig$labels)
    groups.new <- getGroups(plot)
    print(groups.new$colors)

    if(identical(groups.new$colors, groups.new$colors)) {
      colors.new <- c(unique(groups.new$colors)[-1], unique(groups.new$colors)[1])
      plot <- plot + ggplot2::scale_fill_manual(values = colors.new,
                                                labels = groups.orig$labels)
    }
    print(groups.new$colors)

    # for(i in 1:3) {
    #   # Cycle the order of the colors
    #   colors.new <- c(unique(groups.new$colors)[-1], unique(groups.new$colors)[1])
    #   plot <- plot + ggplot2::scale_fill_manual(values = colors.new,
    #                                             labels = groups.orig$labels)
    #   groups.new <- getGroups(plot)
    #   print(groups.new$colors)
    #   print(identical(groups.new$colors, groups.orig$colors))
    # }
    show(plot)
    if(select.list(c('Yes','No'), title = 'Use these color assignments?')=='Yes') edits$cycleColors <- F
    else groups$colors <- colors.new
  }

  if(edits$changeLabels) {
    groups.orig <- getGroups(plot)
    labels.new <- lapply(groups.orig$labels, function(l) readline(paste0('New label to replace "', l, '": ')))
    # names(colors.current) <- labels.new

    # plot <- plot + ggplot2::scale_fill_manual(labels = labels.new,
    #                                           values = colors.current)

    # plot <- plot + ggplot2::scale_fill_discrete(labels = labels.new,
    #                                             type = colors.current)

    # plot <- plot+
    #   ggplot2::scale_fill_discrete(labels = lapply(labels.current,
    #                                                function(l) readline(paste0('New label to replace "', l, '": '))),
    #                                type = colors.current)

    # WTF is going on
    # plot <- plot + ggplot2::scale_fill_discrete(labels = labels.new)

    colors.new <- c(unique(groups.orig$colors[-1]), unique(groups.orig$colors[1]))
    plot <- plot + ggplot2::scale_fill_manual(labels = labels.new,
                                              values = colors.new)

    show(plot)
  }

  return(plot)
}

getGroups <- function(plot) {
  if(!('ggplot' %in% class(plot))) stop('"plot" must be a "ggplot" object')

  plot.data <- ggplot2::ggplot_build(plot)
  groups <- list(colors = plot.data$data[[1]]$fill,
                 labels = plot$scales$scales[unlist(lapply(plot$scales$scales,
                                                           function(n) any(grepl('fill', n$aesthetics))))][[1]]$labels)

  return(groups)
}
