#' Get Hazard Ratios
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in @data containing time-to-event values
#' @param event.status Name, in string format, of column in @data containing event status values (as either 0/1 or T/F)
#' @param predictor_formula Arithmetic combination of desired predictor variables in string format. Must be column names in @data
#' @param time.unit Specify how the time units are displayed on KM graph. Defaults to days
#' @param sig.test Specify type of statistical test to use when reporting p-value for formulas containing only 1 variable.
#'    "logtest" uses log likelihood and "waldtest" uses Wald testing
#' @param showCI Whether or not to show confidence interval ribbons on KM plot. Defaults to True
#' @param showPVal Whether or not to show p-value on KM plot. Defaults to False
#' @param showRiskTab Whether or not to show risk table below KM plot. Defaults to True
#'
#' @return Plots a Kaplan-Meier plot and returns dataframe of Hazard Ratios and associated confidence intervals and p-values
#' @export
#'
getHRs <- function(data,
                   event.time,
                   event.status,
                   predictor_formula,
                   time.unit = c('Days','Weeks','Months','Years'),
                   showCI = T, showPVal = F, showRiskTab = T,
                   sig.test = c('logtest','waldtest')) {
  event.time <- event.time[event.time %in% colnames(data)]
  event.status <- event.status[event.status %in% colnames(data)]

  vars <- getFormulaVars(predictor_formula)
  na_terms <- vars[!(vars %in% colnames(data))]
  if(length(na_terms)) stop(paste('The following terms are not variables (column names) in the data:',
                                  paste(na_terms, sep = ',')))

  data[[event.time]] <- as.numeric(data[[event.time]])
  data[[event.status]] <- as.numeric(data[[event.status]])
  data.plot <- data

  data[vars] <- sapply(data[vars], as.numeric)

  surv <- survival::Surv(data[[event.time]], data[[event.status]])

  f <- formula(paste('surv ~',predictor_formula))
  fit <- survival::coxph(f, data = data)

  if(length(vars) > 1) {
    res <- data.frame(t(sapply(vars, function(v) data.frame('HR'=unname(exp(coef(fit)[[v]])),
                                                            'CI.lower'=exp(confint(fit)[v,1]),
                                                            'CI.upper'=exp(confint(fit)[v,2]),
                                                            'p'=summary(fit)$coefficients[v,'Pr(>|z|)']))))
    colnames(res) <- c('Hazard Ratio',
                       'CI lower',
                       'CI upper',
                       'p-value')
  } else {
    res <- data.frame(matrix(ncol = 0,nrow = 1))

    res[['Hazard Ratio']] <- unname(exp(coef(fit)))
    res[['CI lower']] <- exp(confint(fit))[1]
    res[['CI upper']] <- exp(confint(fit))[2]

    if(missing(sig.test)) sig.test <- 'logtest'
    res[['p-value']] <- summary(fit)[[sig.test]]['pvalue']

    rownames(res) <- vars
  }

  p.loglik <- as.numeric(summary(fit)$logtest['pvalue'])
  p.wald <- as.numeric(summary(fit)$waldtest['pvalue'])

  if(missing(time.unit)) time.unit <- 'Days'

  survfit <- ggsurvfit::survfit2(f, data = data.plot)

  p <- ggsurvfit::ggsurvfit(survfit)+
    ggplot2::labs(x=time.unit,y=event.status)

  if(showCI) p <- p+ggsurvfit::add_confidence_interval()
  if(showPval) p <- p+ggsurvfit::add_pvalue()
  if(showRiskTab) p <- p+ggsurvfit::add_risktable()

  show(p)

  return(list('Statistics'=res,
              'Log Likelihood p-value'=p.loglik,
              'Wald test p-value'=p.wald,
              'KM plot'=p))
}
