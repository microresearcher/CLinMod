
#' Plot ORs
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot.
#' @param var.rename Named vector to rename covariates with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded').
#' @param title Title of the generated figure.
#' @param var_label_position When the plot is split by variables, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left".
#'
#' @return Returns a forest plot of the odds ratios for the predictors in the @model.
#' @export
#'
plotORs <- function(model, n_breaks=7, var.rename=c(), title=NULL, var_label_position='right') {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  res <- getORs.LM(model, repeatVar = T)

  res <- res[res$Variable != '(Intercept)',]
  res[3:6] <- sapply(res[3:6], as.numeric)
  res[is.na(res)] <- 1

  # Using 'unlist' incase user passed a list with 'list()' instead of named vector with 'c()'
  var.rename <- unlist(var.rename[names(var.rename) %in% res$Variable])
  if(length(var.rename)) res$Variable <- stringr::str_replace_all(res$Variable, var.rename)

  # This is to force ggplot2 to order the variable groups as they are in the table, with the help of tidytext package
  res$order <- rev(1:nrow(res))

  p <- ggplot2::ggplot(data = res, ggplot2::aes(y=tidytext::reorder_within(Level, order, Variable), x=OR, label=Level))+
    ggplot2::geom_point(size=4, shape=15)+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=`2.5%`, xmax=`97.5%`), height=.3)+
    ggplot2::geom_vline(xintercept=1, linetype='longdash')+
    ggplot2::facet_wrap(~Variable, ncol=1, scales = 'free_y', strip.position = var_label_position)+
    ggplot2::scale_x_continuous(trans = 'log',
                                breaks = scales::log_breaks(n = n_breaks),
                                labels = scales::label_number(accuracy = 0.1),
                                expand = c(0.1,0.1))+
    tidytext::scale_y_reordered()+
    theme_sa()+
    ggplot2::theme(axis.title.y = ggplot2::element_blank())+
    ggplot2::labs(x='Odds Ratio (Log-scaled)')

  if(var_label_position %in% c('top','bottom')) p <- p+ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))
  if(!is.null(title)) p <- p+ggplot2::labs(title=title)

  return(p)
}


#' Local ggplot2 theme
#'
#'
theme_sa <- function() {
  ggplot2::theme_bw()+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 30),
                   axis.title = ggplot2::element_text(size=25),
                   axis.text = ggplot2::element_text(size=22),
                   strip.text = ggplot2::element_text(size=25),
                   strip.background = ggplot2::element_rect(fill = 'white', color = NA))
}
