
#' Plot ORs
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param data Data table in data.frame format to calculate OR directly from the data using @variable and @response arguments
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param predictors Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values or continuous values
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param subgroup_by (Optional) Vector of variable names for subgroup analyses. Must be column names in data that are not in predictors
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot. Passed onto internal plotForestPlot function.
#' @param var.rename Named vector to rename predictors with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded'). Passed onto internal plotForestPlot function.
#' @param title Title of the generated figure. Passed onto internal plotForestPlot function.
#' @param var_label_position When the plot is split by predictors, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left". Passed onto internal plotForestPlot function.
#' @param annotateStats Whether to add annotations for the HR values and confidence intervals. Defaults to True
#' @param annotatePVal Whether to add annotations for p-values for each group. Defaults to False
#' @param annotateEffect Whether to annotations "helpful" and "harmful" on each side of the y-axis. Defaults to False
#'
#' @return Returns a forest plot of the odds ratios for the predictors in the @model.
#' @export
#'
plotORs <- function(model = NULL,
                    data = NULL, response = NULL, predictors = NULL, family = 'binomial',
                    subgroup_by = NULL,
                    n_breaks=7, var.rename=c(' ' = ' '), title=NULL, var_label_position='top',
                    annotateStats = T, annotatePVal = F, annotateEffect = F) {
  res <- getORs(model = model,
                data = data, response = response, predictors = predictors, family = family,
                subgroup_by = subgroup_by,
                repeatVar = T)

  # Clean up statistics data.frame to pass onto plotting function
  res <- res[res$Variable != '(Intercept)',]
  res[!complete.cases(res),
      c('Odds Ratio','CI lower','CI upper')] <- 1

  colnames(res)[(which(colnames(res)=='Odds Ratio'))] <- 'OR'

  if(length(res$Subvariable)) {
    res$Subvariable <- stringr::str_replace_all(paste0(stringr::str_replace_all(res$Subvariable, var.rename),
                                                       ' (', res$Sublevel, ')'),
                                                c(' \\(\\)' = ''))

    res.list <- lapply(unique(res$Subvariable),
                       function(subv) res[which(res$Subvariable == subv),])
    names(res.list) <- unique(res$Subvariable)
  } else res.list <- list(All = res)

  p.list <- lapply(res.list, function(res.grp) {
    # Force a title to show if this is a subgrouped analysis
    if(length(res.grp$Subvariable)) title <- unique(res.grp$Subvariable)
    p <- plotForestPlot(data = res.grp,
                        n_breaks = n_breaks,
                        var.rename = var.rename,
                        title = title,
                        var_label_position = var_label_position,
                        statistic = 'OR',
                        annotateStats = annotateStats,
                        annotatePVal = annotatePVal,
                        annotateEffect = annotateEffect)

    if(inherits(p, 'patchwork')) p[[1]] <- p[[1]] + ggplot2::labs(x = 'Odds Ratio (Log-scaled)')
    else p <- p + ggplot2::labs(x = 'Odds Ratio (Log-scaled)')

    return(p)
  })

  names(p.list) <- names(res.list)

  return(p.list)
}

#' Plot HRs
#'
#' @param data Data table in data.frame format.
#' @param event.time Name, in string format, of column in data containing time-to-event values.
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F).
#' @param predictors Either a vector of 1 or more predictor variables or an arithmetic combination of predictor variables in character format
#' @param subgroup_by (Optional) Vector of variable names for subgroup analyses. Must be column names in data that are not in predictors
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot. Passed onto internal plotForestPlot function.
#' @param var.rename Named vector to rename predictors with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded'). Passed onto internal plotForestPlot function.
#' @param title Title of the generated figure. Passed onto internal plotForestPlot function.
#' @param var_label_position When the plot is split by predictors, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left". Passed onto internal plotForestPlot function.
#' @param annotateStats Whether to add annotations for the HR values and confidence intervals. Defaults to True
#' @param annotatePVal Whether to add annotations for p-values for each group. Defaults to False
#' @param annotateEffect Whether to annotations "helpful" and "harmful" on each side of the y-axis. Defaults to False
#'
#' @return Returns a forest plot of the hazard ratios for the predictors in the @model.
#' @export
#'
plotHRs <- function(data, event.time, event.status, predictors, subgroup_by = NULL,
                    n_breaks = 7, var.rename = c(' ' = ' '), title = NULL, var_label_position = 'top',
                    annotateStats = T, annotatePVal = F, annotateEffect = F) {
  res <- getHRs(data = data,
                event.time = event.time,
                event.status = event.status,
                predictors = predictors,
                subgroup_by = subgroup_by,
                repeatVar = T)$Statistics

  # Clean up statistics data.frame to pass onto plotting function
  res[!stats::complete.cases(res), c('Hazard Ratio','CI lower','CI upper')] <- 1

  colnames(res)[(which(colnames(res)=='Hazard Ratio'))] <- 'HR'

  # Remove any levels with infinite confidence interval values
  inf_levels <- res[is.infinite(res$`CI lower`) | is.infinite(res$`CI upper`),]$Level
  if(length(inf_levels)) message('The following levels were not plotted due to undefined confidence intervals:\n  ',
                                 paste(inf_levels, collapse = '\n  '))

  if(length(res$Subvariable)) {
    res$Sublevel[res$Sublevel == res$Subvariable] <- ''
    res$Subvariable <- stringr::str_replace_all(paste0(stringr::str_replace_all(res$Subvariable, var.rename),
                                                       ' (', res$Sublevel, ')'),
                                                c(' \\(\\)' = ''))

    res.list <- lapply(unique(res$Subvariable),
                       function(subv) res[which(res$Subvariable == subv),])
    names(res.list) <- unique(res$Subvariable)
  } else res.list <- list(All = res)

  p.list <- lapply(res.list, function(res.grp) {
    # Force a title to show if this is a subgrouped analysis
    if(length(res.grp$Subvariable)) title <- unique(res.grp$Subvariable)
    p <- plotForestPlot(data = res.grp,
                        n_breaks = n_breaks,
                        var.rename = var.rename,
                        title = title,
                        var_label_position = var_label_position,
                        statistic = 'HR',
                        annotateStats = annotateStats,
                        annotatePVal = annotatePVal,
                        annotateEffect = annotateEffect)

    if(inherits(p, 'patchwork')) p[[1]] <- p[[1]] + ggplot2::labs(x='Hazard Ratio (Log-scaled)')
    else p <- p + ggplot2::labs(x='Hazard Ratio (Log-scaled)')

    return(p)
  })

  names(p.list) <- names(res.list)

  return(p.list)
}

#' Internal Forest Plot Function
#'
#' @param data Data for plot, with values and lower and upper bounds of confidence intervals for each level within each variable. Does not need to include baseline.
#' @param n_breaks Number of axis ticks to create on the x-axis of the plot.
#' @param var.rename Named vector to rename predictors with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputting "c('responded_to_treatment'='Responded').
#' @param title Title of the generated figure.
#' @param var_label_position When the plot is split by predictors, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left".
#' @param annotateStats Whether to add annotations for the stat results (e.g. OR, HR, etc) and confidence intervals. Defaults to False
#' @param annotatePVal Whether to add annotations for p-values for each group. Defaults to False
#' @param annotateEffect Whether to annotations "helpful" and "harmful" on each side of the y-axis. Defaults to False
#'
#' @return Forest plot returned to the calling function, either plotORs or plotHRs
plotForestPlot <- function(data, n_breaks, var.rename=c(' ' = ' '), title=NULL, var_label_position='top',
                           annotateStats = F, annotatePVal = F, annotateEffect = F,
                           statistic = 'Statistic') {
  # Using 'unlist' in case user passed a list with 'list()' instead of named vector with 'c()'
  var.rename <- unlist(var.rename[names(var.rename) %in% data$Variable])
  if(length(var.rename)) data$Variable <- stringr::str_replace_all(data$Variable, var.rename)

  # This is to force ggplot2 to order the variable groups as they are in the table
  #   with the help of tidytext package
  data$order <- rev(1:nrow(data))

  # Generate the forest plot
  p <- ggplot2::ggplot(data = data, ggplot2::aes(y = tidytext::reorder_within(Level, order, Variable),
                                                 x = .data[[statistic]],
                                                 label = Level))+
    ggplot2::geom_point(size = 4, shape = 15)+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = `CI lower`, xmax = `CI upper`), height = 0.3)+
    ggplot2::geom_vline(xintercept = 1, linetype = 'longdash')+
    ggplot2::facet_wrap(~Variable, ncol = 1, scales = 'free_y', strip.position = var_label_position)+
    ggplot2::scale_x_continuous(trans = 'log',
                                breaks = scales::log_breaks(n = n_breaks),
                                labels = scales::label_number(accuracy = 0.1),
                                expand = c(0.1, 0.1))+
    tidytext::scale_y_reordered()+
    theme_sa()+
    ggplot2::theme(axis.title.y = ggplot2::element_blank())

  if(!is.null(title)) p <- p+ggplot2::labs(title = title)

  p.layout <- c(patchwork::area(t = 0, l = 0, b = 30, r = 6))

  # Add statistics with confidence intervals to the side of the plot
  if(annotateStats) {
    data.stats <- cbind(data[c('Variable','Level','order')],
                        c(ifelse(is.na(data$`p-value`), '',
                                 paste0(signif(data[[statistic]], digits = 3),
                                        ' (',signif(data$`CI lower`, digits = 3),
                                        ' - ',
                                        signif(data$`CI upper`, digits = 3),')'))))
    colnames(data.stats)[ncol(data.stats)] <- statistic

    p.layout <- c(p.layout, patchwork::area(t = 0, l = 7, b = 30, r = 9))

    p <- p+ggplot2::ggplot(data = data.stats, ggplot2::aes(y = reorder(Level, order)))+
      ggplot2::geom_text(ggplot2::aes(x = 1, label = .data[[statistic]]), size = 18*5/14)+
      ggplot2::facet_wrap(~Variable, ncol = 1, scales = 'free_y', strip.position = var_label_position)+
      ggplot2::theme_void()+
      ggplot2::labs(title = paste(statistic, '(95% CI)'))+
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5, face = 'bold'),
                     strip.text = ggplot2::element_text(size = 25, color = 'white'))+
      patchwork::plot_layout(design = p.layout)
  }

  # Add p-values to the side of the plot
  if(annotatePVal) {
    data.pval <- cbind(data[c('Variable','Level','order')],
                       c(ifelse(is.na(data$`p-value`), '',
                                ifelse(data$`p-value` < 0.01,
                                       paste0(format(data$`p-value`, scientific = T, digits = 3)),
                                       paste0(signif(data$`p-value`, digits = 3))))))
    colnames(data.pval)[ncol(data.pval)] <- 'p'

    p.layout <- c(p.layout, patchwork::area(t = 0, l = 9.5, b = 30, r = 12))

    p <- p+ggplot2::ggplot(data = data.pval, ggplot2::aes(y = reorder(Level, order)))+
      ggplot2::geom_text(ggplot2::aes(x = 1, label = .data$p), size = 18*5/14)+
      ggplot2::facet_wrap(~Variable, ncol = 1, scales = 'free_y', strip.position = var_label_position)+
      ggplot2::theme_void()+
      ggplot2::labs(title = 'p-value')+
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5, face = 'bold'),
                     strip.text = ggplot2::element_text(size = 25, color = 'white'))+
      patchwork::plot_layout(design = p.layout)
  }

  # if(annotateEffect) p <- p+ggplot2::coord_cartesian(ylim = c(1, nrow(data) + 1))
  if(var_label_position %in% c('top','bottom')) p <- p+ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))

  return(p)
}

#' Local ggplot2 theme
#'
theme_sa <- function() {
  ggplot2::theme_bw()+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 30),
                   axis.title = ggplot2::element_text(size = 25),
                   axis.text = ggplot2::element_text(size = 22),
                   strip.text = ggplot2::element_text(size = 25),
                   strip.background = ggplot2::element_rect(fill = 'white', color = NA))
}
