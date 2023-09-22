#' Get Hazard Ratios
#'
#' @param data Data table in data.frame format
#' @param event.time Name of column, in string format, containing time-to-event values
#' @param event.status Name of column, in string format, containing event status values (as either 0/1 or T/F)
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param time.unit Specify how the time units are displayed on KM graph. Defaults to days
#' @param sig.test Specify type of statistical test to use
#'
#' @return Plots a Kaplan-Meier plot and returns dataframe of Hazard Ratios and associated confidence intervals and p-values
#' @export
#'
getHRs <- function(data,
                   event.time,
                   event.status,
                   variable,
                   time.unit = c('days','weeks','months','years'),
                   sig.test = c('loglik','wald')) {
  event.time <- event.time[event.time %in% colnames(data)]
  event.status <- event.status[event.status %in% colnames(data)]
  var <- variable[variable %in% colnames(data)]

  f <- formula(paste('surv ~',var))

  data[[event.time]] <- as.numeric(data[[event.time]])
  data[[event.status]] <- as.numeric(data[[event.status]])

  surv <- survival::Surv(data[[event.time]], data[[event.status]])
  fit <- survival::coxph(f, data = data)

  res <- data.frame(matrix(ncol = 0,nrow = 1))
  res[['Hazard Ratio']] <- unname(exp(coef(fit)))
  res[['CI lower 2.5%']] <- exp(confint(fit))[1]
  res[['CI upper 97.5%']] <- exp(confint(fit))[2]
  # res[['95CI']] <- unlist(as.list(exp(confint(fit))))
  res[['p-value']] <- summary(fit)$logtest['pvalue']

  survfit <- ggsurvfit::survfit2(f, data = data)

  if(missing(time.unit)) time.unit <- 'days'

  ggsurvfit::ggsurvfit(survfit)+
    ggplot2::labs(x=time.unit,y=event.status)+
    ggsurvfit::add_confidence_interval()+
    ggsurvfit::add_pvalue()+
    ggsurvfit::add_risktable()

  return(res)
}
