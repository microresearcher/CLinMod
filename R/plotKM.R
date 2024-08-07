#' Plot Kaplan-Meier plot of Events
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in @data containing time-to-event values
#' @param event.status Name, in string format, of column in @data containing event status values (as either 1/0 or T/F)
#' @param predictor Variable of interest to separate data by into different curves. Must be a column name in @data
#' @param time.unit Specify how the time units are displayed on KM graph. Defaults to days
#' @param convert.time (Optional) Specify an alternate time unit to display on the KM plot
#' @param event.name (Optional) Name to display on y-axis title of KM plot. Will display as "@event.name Probability". Defaults to @event.status
#' @param var.rename Named vector to rename predictors with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputing "c('responded_to_treatment'='Responded'). Passed onto internal plotForestPlot function.
#' @param showCI Whether or not to show confidence interval ribbons on KM plot. Defaults to True
#' @param showPVal Whether or not to show p-value on KM plot. Defaults to False
#' @param showRiskTab Whether or not to show risk table below KM plot. Defaults to False
#' @param offerSave Whether to offer saving the plot to a file. Defaults to True
#' @param width Width of figure, in inches. Decreasing width/height increases relative size of text. Defaults to 8
#' @param height Height of figure, in inches. Decreasing width/height increases relative size of text. Defaults to 4
#'
#' @return Returns a Kaplan-Meier plot
#' @export
#'
plotKM <- function(data,
                   event.time,
                   event.status,
                   predictor,
                   time.unit = c('Days','Weeks','Months','Years'),
                   convert.time = c('Days','Weeks','Months','Years'),
                   event.name,
                   var.rename = c(' ' = ' '),
                   showCI = T, showPVal = F, showRiskTab = F,
                   offerSave = T,
                   width = 8, height = 4) {
  event.time <- event.time[event.time %in% colnames(data)]
  event.status <- event.status[event.status %in% colnames(data)]
  predictor <- predictor[predictor %in% colnames(data)]
  if(missing(event.name)) event.name <- event.status

  if(!length(event.time)) stop(paste0('"',event.time,'" must be a column name in data'))
  if(!length(event.status)) stop(paste0('"',event.status,'" must be a column name in data'))
  if(!length(predictor)) stop(paste0('"',predictor,'" must be a column name in data'))

  f <- as.formula(paste('surv ~', predictor))

  time.unit <- match.arg(time.unit)
  convert.time <- match.arg(convert.time)

  data[[event.time]] <- as.numeric(data[[event.time]])

  if(time.unit != convert.time) {
    unit.factors <- list('Days'=365.24, 'Weeks'=52, 'Months'=12, 'Years'=1)
    data[[event.time]] <- data[[event.time]] * unit.factors[[convert.time]] / unit.factors[[time.unit]]
    time.unit <- convert.time
  }

  data[[event.status]] <- as.numeric(data[[event.status]])

  var.rename <- var.rename[intersect(names(var.rename), levels(data[[predictor]]))]
  if(length(var.rename)) data[[predictor]] <- plyr::revalue(data[[predictor]], var.rename)

  surv <- survival::Surv(data[[event.time]], data[[event.status]])
  survfit <- ggsurvfit::survfit2(formula = f, data = data)

  p <- ggsurvfit::ggsurvfit(survfit, size = 1, theme = ggpubr::theme_pubr())+
    ggsurvfit::add_censor_mark(stroke = 1)+
    ggplot2::labs(x=paste0('Time (', time.unit,')'),
                  y=paste(event.name,'Probability (%)'))+
    ggplot2::theme(axis.title = ggplot2::element_text(size = 20),
                   axis.text = ggplot2::element_text(size = 20),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 18),
                   legend.key.size = ggplot2::unit(1,'cm'))+
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01),
                                labels = function(x) paste(x * 100))

  if(showCI) p <- p+ggsurvfit::add_confidence_interval()
  if(showPVal) p <- p+ggsurvfit::add_pvalue(size = 10)
  if(showRiskTab) {
    p <- p+ggsurvfit::add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                                    size = 5,
                                    theme = ggsurvfit::theme_risktable_default(axis.text.y.size = 15,
                                                                               plot.title.size = 15))+
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12)))+
      ggplot2::geom_vline(xintercept = 0, linetype = 'dashed')

    height <- height * 1.6
  }

  width <- width * 1.1

  if(offerSave) savePlot(p, width = width, height = height)

  logrank <- survival::survdiff(formula = f, data = data)
  print(logrank)
  return(p)
}
