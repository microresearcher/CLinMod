
#' Plot response frequency (contingency table values) for a single binary variable
#'
#' @param data Data table in data.frame format
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param proportional Whether to display as absolute (if set to False) or proportional values (if set to True)
#' @param variable_print (Optional) Name of the variable to display on the figure if different from the column name in @data@
#' @param response_print (Optional) Name of the response to display on the figure if different from the column name in @data@
#' @param flip Whether to flip the x and y axes of the figure
#' @param grp_colors (Optional) A list or named list of colors to use in the figure
#' @param includeNA Whether or not to include NAs in the figure
#'
#' @return Plot in ggplot format
#' @export
#'
plotFreq <- function(data,
                     variable,
                     response,
                     variable_print=NULL,
                     response_print=NULL,
                     proportional=F,
                     flip=F,
                     includeNA=F,
                     grp_colors=NULL) {
  if(is.null(variable_print)) variable_print <- variable
  if(is.null(response_print)) response_print <- response

  if(flip) {
    y <- list(name = variable, print = variable_print)
    x <- list(name = response, print = response_print)
  } else {
    x <- list(name = variable, print = variable_print)
    y <- list(name = response, print = response_print)
  }

  temp <- table(data[[x$name]], data[[y$name]])
  freqtab <- data.frame(rownames(temp),
                        temp[,1],
                        temp[,2])
  colnames(freqtab) <- c(x$name, colnames(temp))
  rownames(freqtab) <- NULL

  freqtab$Totals <- rowSums(freqtab[c(colnames(temp))])
  print(freqtab)

  plottab <- freqtab %>%
    tidyr::pivot_longer(c(colnames(temp))) %>%
    dplyr::relocate(Totals, .after = dplyr::last_col())

  plottab[plottab == 'NA'] <- NA
  colnames(plottab)[c(2,3)] <- c(y$name, 'Count')
  plottab$Percent <- plottab$Count/plottab$Totals * 100

  if(proportional) {
    p <- ggpubr::ggbarplot(plottab, x=x$name, y='Percent',
                           fill=y$name, color='white')+
      ggplot2::labs(y = 'Proportion (%)')
  } else {
    p <- ggpubr::ggbarplot(plottab, x=x$name, y='Count',
                           fill=y$name, color='white')+
      ggplot2::labs(y = 'Count')
  }

  grp_colors <- grp_colors[grp_colors %in% colors()][1:length(unique(plottab[[y$name]]))]
  if(any(is.na(grp_colors))) grp_colors <- unique(ggplot2::ggplot_build(p)$data[[1]][["fill"]])

  p <- p +
    ggplot2::labs(x = x$print)+
    scale_fill_manual(values = grp_colors)+
    ggplot2::theme(axis.title = ggplot2::element_text(size=25),
          axis.text = ggplot2::element_text(size=22),
          legend.position = 'right',
          legend.title = ggplot2::element_text(size = 22),
          legend.text = ggplot2::element_text(size=15),
          legend.key.size = ggplot2::unit(1,'cm'))

  while(ifelse(select.list(title = 'Would you like to switch the colors?',
                           choices = c('Yes','No'))=='Yes', T, F)) {
    grp_colors <- c(grp_colors[-1], grp_colors[1])
    p <- p+ggplot2::scale_fill_manual(values = grp_colors)
    print(p)
  }

  if(select.list(title = 'Save figure?', choices = c('Yes', 'No')) == 'Yes') {
    ggplot2::ggsave(file.path(rstudioapi::selectDirectory(),
                              paste0(readline('Figure name: '),'.png')),
                    plot = p,
                    device='png',
                    width=10, height=8, units = 'in',
                    dpi=600)
  }

  return(p)
}
