
#' Plot ORs
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot. Passed onto internal plotForestPlot function.
#' @param var.rename Named vector to rename variables with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded'). Passed onto internal plotForestPlot function.
#' @param title Title of the generated figure. Passed onto internal plotForestPlot function.
#' @param var_label_position When the plot is split by variables, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left". Passed onto internal plotForestPlot function.
#'
#' @return Returns a forest plot of the odds ratios for the predictors in the @model.
#' @export
#'
plotORs <- function(model, n_breaks=7, var.rename=c(), title=NULL, var_label_position='right') {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  res <- getORs.LM(model, repeatVar = T)

  # Clean up statistics data.frame to pass onto plotting function
  res <- res[res$Variable != '(Intercept)',]
  res[!complete.cases(res),c('Odds Ratio','CI lower','CI upper')] <- 1

  # Rename the 'Hazard Ratio' column to a generalized name that plotForestPlot will look for
  colnames(res)[(which(colnames(res)=='Odds Ratio'))] <- 'statistic'

  p <- plotForestPlot(data = res,
                      n_breaks = n_breaks,
                      var.rename = var.rename,
                      title = title,
                      var_label_position = var_label_position)

  p <- p + ggplot2::labs(x='Odds Ratio (Log-scaled)')

  return(p)
}

#' Plot HRs
#'
#' @param data Data table in data.frame format.
#' @param event.time Name, in string format, of column in data containing time-to-event values.
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F).
#' @param predictor_formula Arithmetic combination of desired predictor variables in string format. Must be column names in data.
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot. Passed onto internal plotForestPlot function.
#' @param var.rename Named vector to rename variables with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded'). Passed onto internal plotForestPlot function.
#' @param title Title of the generated figure. Passed onto internal plotForestPlot function.
#' @param var_label_position When the plot is split by variables, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left". Passed onto internal plotForestPlot function.
#'
#' @return Returns a forest plot of the hazard ratios for the predictors in the @model.
#' @export
#'
plotHRs <- function(data, event.time, event.status, predictor_formula,
                    n_breaks=7, var.rename=c(), title=NULL, var_label_position='top') {
  res <- getHRs(data = data,
                event.time = event.time,
                event.status = event.status,
                predictor_formula = predictor_formula,
                repeatVar = T)$Statistics

  # Clean up statistics data.frame to pass onto plotting function
  res[!stats::complete.cases(res), c('Hazard Ratio','CI lower','CI upper')] <- 1

  # Rename the 'Hazard Ratio' column to a generalized name that plotForestPlot will look for
  colnames(res)[(which(colnames(res)=='Hazard Ratio'))] <- 'statistic'

  p <- plotForestPlot(data = res,
                      n_breaks = n_breaks,
                      var.rename = var.rename,
                      title = title,
                      var_label_position = var_label_position)

  p <- p + ggplot2::labs(x='Hazard Ratio (Log-scaled)')

  return(p)
}

#' Internal Forest Plot Function
#'
#' @param data Data for plot, with values and lower and upper bounds of confidence intervals for each level within each variable. Does not need to include baseline.
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot.
#' @param var.rename Named vector to rename variables with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded').
#' @param title Title of the generated figure.
#' @param var_label_position When the plot is split by variables, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left".
#'
#' @return Forest plot returned to the calling function, either plotORs or plotHRs
plotForestPlot <- function(data, statistic, n_breaks, var.rename, title, var_label_position) {
  # Using 'unlist' in case user passed a list with 'list()' instead of named vector with 'c()'
  var.rename <- unlist(var.rename[names(var.rename) %in% data$Variable])
  if(length(var.rename)) data$Variable <- stringr::str_replace_all(data$Variable, var.rename)

  # This is to force ggplot2 to order the variable groups as they are in the table, with the help of tidytext package
  data$order <- rev(1:nrow(data))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(y=tidytext::reorder_within(Level, order, Variable), x=statistic, label=Level))+
    ggplot2::geom_point(size=4, shape=15)+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=`CI lower`, xmax=`CI upper`), height=.3)+
    ggplot2::geom_vline(xintercept=1, linetype='longdash')+
    ggplot2::facet_wrap(~Variable, ncol=1, scales = 'free_y', strip.position = var_label_position)+
    ggplot2::scale_x_continuous(trans = 'log',
                                breaks = scales::log_breaks(n = n_breaks),
                                labels = scales::label_number(accuracy = 0.1),
                                expand = c(0.1,0.1))+
    tidytext::scale_y_reordered()+
    theme_sa()+
    ggplot2::theme(axis.title.y = ggplot2::element_blank())

  if(var_label_position %in% c('top','bottom')) p <- p+ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))
  if(!is.null(title)) p <- p+ggplot2::labs(title=title)

  return(p)
}

#' Local ggplot2 theme
#'
theme_sa <- function() {
  ggplot2::theme_bw()+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 30),
                   axis.title = ggplot2::element_text(size=25),
                   axis.text = ggplot2::element_text(size=22),
                   strip.text = ggplot2::element_text(size=25),
                   strip.background = ggplot2::element_rect(fill = 'white', color = NA))
}
