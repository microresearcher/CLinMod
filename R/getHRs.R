#' Get Hazard Ratios
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in data containing time-to-event values
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F)
#' @param predictor_formula Arithmetic combination of desired predictor variables in string format. Must be column names in data
#' @return Returns a dataframe of Hazard Ratios and associated confidence intervals and p-values
#' @export
#'
getHRs <- function(data,
                   event.time,
                   event.status,
                   predictor_formula) {
  event.time <- event.time[event.time %in% colnames(data)]
  event.status <- event.status[event.status %in% colnames(data)]

  if(any(!length(event.time), !length(event.status))) stop(paste0('"', event.time,
                                                                  '" and "', event.status,
                                                                  '" must be column names in data'))

  vars <- getFormulaVars(predictor_formula)
  na_terms <- vars[!(vars %in% colnames(data))]
  if(length(na_terms)) stop(paste('The following terms are not variables (column names) in the data:\n '),
                            paste0(na_terms, sep = ', '))

  data[[event.time]] <- as.numeric(data[[event.time]])
  data[[event.status]] <- as.numeric(data[[event.status]])
  # data[vars] <- sapply(data[vars], as.factor)

  surv <- survival::Surv(data[[event.time]], data[[event.status]])

  f <- formula(paste('surv ~', predictor_formula))
  fit <- survival::coxph(f, data = data)

  if(length(vars) > 1) {
    vars.groups <- vars.groups <- lapply(vars, function(v) {
      list(baseline=levels(unique(as.factor(data[[v]])))[1],
           comparisons=levels(unique(as.factor(data[[v]])))[2:length(unique(data[[v]]))])
    })
    names(vars.groups) <- vars

    names(fit$coefficients) <- as.vector(unlist(lapply(vars.groups, function(v) v$comparisons)))

    res <- dplyr::bind_rows(lapply(vars.groups, function(v) {
      temp <- rbind(c('-','-','-','-'),
                    data.frame('HR' = unname(exp(coef(fit)[v$comparisons])),
                               'CI.lower' = exp(confint(fit)[v$comparisons,1]),
                               'CI.upper' = exp(confint(fit)[v$comparisons,2]),
                               'p' = summary(fit)$coefficients[v$comparisons,'Pr(>|z|)']))
      rownames(temp) <- c(v$baseline, v$comparisons)
      return(temp)
    }))

    colnames(res) <- c('Hazard Ratio',
                       'CI lower',
                       'CI upper',
                       'p-value')
  } else {
    baseline <- levels(unique(as.factor(data[[vars]])))[1]
    comparisons <- levels(unique(as.factor(data[[vars]])))[2:length(unique(data[[vars]]))]

    names(fit$coefficients) <- comparisons

    res <- rbind(c('-','-','-','-'),
                 data.frame('HR' = unname(exp(coef(fit)[comparisons])),
                            'CI.lower' = exp(confint(fit)[comparisons,1]),
                            'CI.upper' = exp(confint(fit)[comparisons,2]),
                            'p' = summary(fit)$coefficients[comparisons,'Pr(>|z|)']))
    rownames(res) <- c(baseline, comparisons)
  }

  p.loglik <- as.numeric(summary(fit)$logtest['pvalue'])
  p.wald <- as.numeric(summary(fit)$waldtest['pvalue'])

  return(list('Variables'=vars,
              'Statistics'=res,
              'Log Likelihood p-value'=p.loglik,
              'Wald test p-value'=p.wald))
}
