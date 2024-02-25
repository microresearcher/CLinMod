#' Get Hazard Ratios
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in data containing time-to-event values
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F)
#' @param predictor_formula Arithmetic combination of desired predictor variables in string format. Must use column names in data
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to "FALSE".
#' @return Returns a dataframe of Hazard Ratios and associated confidence intervals and p-values
#' @export
#'
getHRs <- function(data,
                   event.time,
                   event.status,
                   predictor_formula,
                   repeatVar = F) {
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
    # Create a nested list of baseline and comparison levels for each variable
    vars.groups <- vars.groups <- lapply(vars, function(v) {
      # If a variable is not already coded as a factor in the dataframe, then it is not categorical and is instead continuous.
      #   For these variables, there is no baseline comparison and instead there will only be a single row in the resultant dataframe for each of these variables.
      #   For the sake of consistency, for now an extra row will be placed for the 'baseline' and labeled with '-' and the real row will be labeled with the variable name.
      if(!is.factor(data[[v]])) list(baseline = '-', comparisons = v)
      else list(baseline = levels(as.factor(data[[v]]))[1],
                comparisons = levels(as.factor(data[[v]]))[2:length(levels(as.factor(data[[v]])))])
    })
    # Name each sublist with the variable name
    names(vars.groups) <- vars

    # Rename the coefficients in the coxph output since those are written as levels prepended with their variable name
    names(fit$coefficients) <- as.vector(unlist(lapply(vars.groups, function(v) v$comparisons)))

    # Create a data.frame that is easy to read for the user and for a plotting function
    res <- dplyr::bind_rows(lapply(vars.groups, function(v) {
      temp <- rbind(c('-','-','-','-'),
                    data.frame('HR' = unname(exp(coef(fit)[v$comparisons])),
                               'CI.lower' = exp(confint(fit)[v$comparisons,1]),
                               'CI.upper' = exp(confint(fit)[v$comparisons,2]),
                               'p' = summary(fit)$coefficients[v$comparisons,'Pr(>|z|)']))
      rownames(temp) <- c(v$baseline, v$comparisons)
      return(temp)
    }))

    # Rename the column names now that R won't fiddle with whitespaces and special characters
    colnames(res) <- c('Hazard Ratio',
                       'CI lower',
                       'CI upper',
                       'p-value')

    # Add a column for variable names which can be helpful for the user but is important for plotting
    res$Variable <- unlist(lapply(vars, function(v) rep(v, length(vars.groups[[v]]$comparisons)+1)))
  } else {
    # If a variable is not already coded as a factor in the dataframe, then it is not categorical and is instead continuous.
    #   For these variables, there is no baseline comparison and instead there will only be a single row in the resultant dataframe for each of these variables.
    #   For the sake of consistency, for now an extra row will be placed for the 'baseline' and labeled with '-' and the real row will be labeled with the variable name.
    if(!is.factor(data[[vars]])) {
      baseline <- '-'
      comparisons <- vars
    } else {
      baseline <- levels(as.factor(data[[vars]]))[1]
      comparisons <- levels(as.factor(data[[vars]]))[2:length(levels(as.factor(data[[vars]])))]
    }

    names(fit$coefficients) <- comparisons

    res <- rbind(c('-','-','-','-'),
                 data.frame('HR' = unname(exp(coef(fit)[comparisons])),
                            'CI.lower' = exp(confint(fit)[comparisons,1]),
                            'CI.upper' = exp(confint(fit)[comparisons,2]),
                            'p' = summary(fit)$coefficients[comparisons,'Pr(>|z|)']))
    rownames(res) <- c(baseline, comparisons)

    colnames(res) <- c('Hazard Ratio',
                       'CI lower',
                       'CI upper',
                       'p-value')

    res$Variable <- vars
  }

  # Reorder columns to put 'Variable' first, 'Level' second, and remove any rows with '-' in the Level column since these are extra rows for continuous variables
  res <- cbind('Variable' = res$Variable,
               'Level' = rownames(res),
               res[colnames(res) != 'Variable']) %>% dplyr::filter(Level != '-')

  # Remove repeated variable names if repeatVar is false
  if(!repeatVar) res$Variable[duplicated(res$Variable)] <- ''
  rownames(res) <- 1:nrow(res)

  # Remove any NA cases due to variable levels for which no values exist
  res <- res[stats::complete.cases(res),]
  # Make the values numeric, turning baseline rows into NA
  res[c('Hazard Ratio','CI lower','CI upper','p-value')] <- sapply(res[c('Hazard Ratio','CI lower','CI upper','p-value')],
                                                                   function(x) suppressWarnings(as.numeric(x)))

  p.loglik <- as.numeric(summary(fit)$logtest['pvalue'])
  p.wald <- as.numeric(summary(fit)$waldtest['pvalue'])

  return(list('Cox PH Model' = fit,
              'Variables' = vars,
              'Statistics' = res,
              'Log Likelihood p-value' = p.loglik,
              'Wald test p-value' = p.wald))
}
