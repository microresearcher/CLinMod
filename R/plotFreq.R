
#' Plot response frequency (contingency table values) for a single binary variable
#'
#' @param data Data table in data.frame format
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param proportional Whether to display as absolute (if set to False) or proportional values (if set to True)
#' @param variable_print (Optional) Name of the variable to display on the figure if different from the column name in @data@
#' @param response_print (Optional) Name of the response to display on the figure if different from the column name in @data@
#' @param stratify_by (Optional) Name of column, in string format, for a third variable to stratify results and plot by
#' @param flip Whether to flip the x and y axes of the figure
#' @param grp_colors (Optional) A list or named list of colors to use in the figure
#' @param includeNA Whether or not to include NAs in the figure
#' @param separateLegend Whether or not to save legend separately to a different file. Defaults to False
#'
#' @return Plot in ggplot format
#' @export
#'
plotFreq <- function(data,
                     variable,
                     response,
                     variable_print=NULL,
                     response_print=NULL,
                     stratify_by=NULL,
                     proportional=F,
                     flip=F,
                     separateLegend=F,
                     includeNA=F,
                     grp_colors=NA) {
  if(is.null(variable_print)) variable_print <- variable
  if(is.null(response_print)) response_print <- response
  stratify_by <- stratify_by[stratify_by %in% colnames(data)]

  if(flip) {
    y <- list(name = variable, print = variable_print)
    x <- list(name = response, print = response_print)
  } else {
    x <- list(name = variable, print = variable_print)
    y <- list(name = response, print = response_print)
  }

  # Create an overall contingency table regardless of whether data is being stratified
  temp <- table(data[[x$name]], data[[y$name]])

  freqtab <- data.frame(rownames(temp),
                        temp[,1],
                        temp[,2])
  colnames(freqtab) <- c(x$name, colnames(temp))
  rownames(freqtab) <- NULL

  freqtab$Totals <- rowSums(freqtab[c(colnames(temp))])

  print(freqtab)

  # Now calculate stratified contingency table if requested
  if(length(stratify_by)==1) {
    data[[stratify_by]] <- as.factor(data[[stratify_by]])
    freqtabs <- lapply(levels(data[[stratify_by]]), function(stratum) {
      temp <- table(data[data[stratify_by]==stratum,][[x$name]],
                    data[data[stratify_by]==stratum,][[y$name]])
      freqtab.stratum <- data.frame(rownames(temp),
                                    temp[,1],
                                    temp[,2])
      colnames(freqtab.stratum) <- c(x$name, colnames(temp))
      rownames(freqtab.stratum) <- NULL
      freqtab.stratum$Totals <- rowSums(freqtab.stratum[c(colnames(temp))])
      return(freqtab.stratum)
    })
    names(freqtabs) <- levels(data[[stratify_by]])
    freqtab <- dplyr::bind_rows(freqtabs, .id = stratify_by)
    print(freqtab)
  } else if(!is.null(stratify_by)) stop('Can only currently stratify across one variable')

  plottab <- freqtab %>%
    tidyr::pivot_longer(c(colnames(temp))) %>%
    dplyr::relocate(Totals, .after = dplyr::last_col())
  colnames(plottab)[c(ncol(plottab)-2,ncol(plottab)-1)] <- c(y$name, 'Count')

  plottab[plottab == 'NA'] <- NA
  plottab$Percent <- plottab$Count/plottab$Totals * 100

  if(proportional) {
    p <- ggpubr::ggbarplot(plottab, x=x$name, y='Percent',
                           fill=y$name, color='white',
                           facet.by = stratify_by, nrow = 1)+
      ggplot2::labs(y = 'Proportion (%)')
  } else {
    p <- ggpubr::ggbarplot(plottab, x=x$name, y='Count',
                           fill=y$name, color='white',
                           facet.by = stratify_by, nrow = 1)+
      ggplot2::labs(y = 'Count')
  }

  grp_colors <- grp_colors[grp_colors %in% colors()][1:length(unique(plottab[[y$name]]))]
  if(any(is.na(grp_colors))) grp_colors <- unique(ggplot2::ggplot_build(p)$data[[1]][["fill"]])

  p <- p +
    ggplot2::labs(x = x$print)+
    ggplot2::scale_fill_manual(values = grp_colors, name = y$print)+
    ggplot2::theme(axis.title = ggplot2::element_text(size=25),
          axis.text = ggplot2::element_text(size=22),
          legend.position = 'right',
          legend.title = ggplot2::element_text(size = 22),
          legend.text = ggplot2::element_text(size=20),
          legend.key.size = ggplot2::unit(1,'cm'),
          strip.text.x = ggplot2::element_text(size=20))

  print(p)

  while(select.list(title = '\nWould you like to switch the colors?', choices = c('Yes','No')) == 'Yes') {
    grp_colors <- c(grp_colors[-1], grp_colors[1])
    p <- p+ggplot2::scale_fill_manual(values = grp_colors)
    print(p)
  }

  if(select.list(title = '\nSave figure?', choices = c('Yes', 'No')) == 'Yes') {
    ggplot2::ggsave(filename = file.path(rstudioapi::selectDirectory(),
                              paste0(readline('Figure name: '),'.png')),
                    plot = p,
                    device='png',
                    width=10, height=8, units = 'in',
                    dpi=600)
  }

  cat(paste0('\nTo modify this figure, save the output of this function to a variable (for example, "p")\nand then perform any of the following as needed',
               '\n  Change x-axis text: "p + ggplot2::scale_x_discrete(labels = c([text1], [text2], etc)"',
               '\n  Change legend text: "p + ggplot2::scale_fill_manual(labels = c([text1], [text2], etc)"',
               '\n  Remove x-axis title: "p + ggplot2::theme(axis.title.x = element_blank())"',
               '\n  Remove legend title: "p + ggplot2::theme(legend.title = element_blank())"'))
  return(p)
}

#' Get "fill" colors from a ggplot object
#'
#' @param plot A ggplot object
#'
#' @return Returns the colors used in the "fill" of the ggplot
#' @export
#'
getFillColors <- function(plot) {
  return(unique(ggplot2::ggplot_build(p)$data[[1]][["fill"]]))
}
