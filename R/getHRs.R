#' Get Hazard Ratios
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in data containing time-to-event values
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F)
#' @param predictors Either a vector of 1 or more predictor variables or an arithmetic combination of predictor variables in character format
#' @param subgroup_by (Optional) Vector of variable names for subgroup analyses. Must be column names in data that are not in predictor_formula.
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to False
#' @param verbose Whether to print extra information during processing and analysis. Defaults to True.
#'
#' @return Returns a dataframe of Hazard Ratios and associated confidence intervals and p-values
#' @export
#'
getHRs <- function(data, event.time, event.status,
                   predictors,
                   subgroup_by = NULL,
                   repeatVar = F,
                   verbose = T) {
  event.time <- intersect(event.time, colnames(data))
  event.status <- intersect(event.status, colnames(data))
  subgroups <- intersect(subgroup_by, colnames(data))

  if(!all(length(event.time), length(event.status))) stop(paste0('"', event.time,
                                                                 '" and "', event.status,
                                                                 '" must be column names in data'))

  # If length of predictors is greater than 1, then it should be a vector/list of predictors
  # If length of predictors is 1, then it should be either a formula or a single variable
  if(length(predictors) > 1) vars <- predictors
  else vars <- getFormulaVars(predictors)

  # If any predictors specified are not found in the data then function will error out
  na_terms <- setdiff(vars, colnames(data))
  if(length(na_terms)) stop(paste('The following terms are not variables (column names) in the data:\n '),
                            paste0(na_terms, sep = ', '))
  else if(length(intersect(subgroups, vars))) stop('Stratification variable cannot be in the predictor formula.')

  # Paste predictors into a formula string. If a formula was already provided, this does not change it
  predictor_formula <- paste0(predictors, collapse = ' + ')

  data[[event.time]] <- as.numeric(data[[event.time]])
  data[[event.status]] <- as.numeric(data[[event.status]])

  if(length(subgroups)) {
    # Univariate Analysis with Subgroups
    res.subgrps <- lapply(subgroups, function(subgrp) {
      subgrps.n <- data[c(vars, subgrp)] %>%
        dplyr::mutate('count' = 1) %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(vars),
                           values_from = 'count',
                           values_fn = sum, values_fill = 0)

      # if(verbose) for(i in 1:sum(!is.na(subgrps.n[[subgrp]]))) {
      #   cat()
      # }
      if(verbose) print(subgrps.n)

      grouped <- data[complete.cases(data[c(event.time,
                                            event.status,
                                            vars,
                                            subgrp)]),] %>%
        dplyr::group_by(.data[[subgrp]])

      res.grouped <- grouped %>%
        dplyr::group_map(~ getHRs.base(.x,
                                       event.time = event.time,
                                       event.status = event.status,
                                       predictor_formula = paste(vars, collapse = ' + '),
                                       repeatVar = T)$Statistics)

      names(res.grouped) <- (grouped %>% dplyr::group_keys())[[1]]

      res.sub <- dplyr::bind_rows(lapply(names(res.grouped)[!is.na(names(res.grouped))],
                                         function(grp) cbind(Lvl = grp, Var = subgrp, res.grouped[[grp]])))

      res.sub <- res.sub[complete.cases(res.sub),]

      inf_grps <- apply(res.sub[c('Hazard Ratio', 'CI lower', 'CI upper', 'p-value')],
                        MARGIN = 1,
                        FUN = function(grp) any(is.infinite(grp)))

      if(length(inf_grps[inf_grps])) {
        warning(' The following subgroups had infinite values:\n   ',
                paste0(lapply(names(inf_grps[inf_grps]),
                              function(grp) paste0(res.sub[grp,]$Variable,
                                                   ':', res.sub[grp,]$Level,
                                                   ' within ',
                                                   res.sub[grp,]$Var,
                                                   ':', res.sub[grp,]$Lvl)),
                       collapse = '\n   '))
      }

      res.sub <- res.sub[!inf_grps,]

      colnames(res.sub)[colnames(res.sub) == 'Level'] <- 'Sublevel'
      colnames(res.sub)[colnames(res.sub) == 'Lvl'] <- 'Level'
      colnames(res.sub)[colnames(res.sub) == 'Variable'] <- 'Subvariable'
      colnames(res.sub)[colnames(res.sub) == 'Var'] <- 'Variable'
      rownames(res.sub) <- NULL

      return(res.sub)
    })

    res <- list('Statistics' = dplyr::bind_rows(res.subgrps))
  } else {
    # Univariate or Multivariate Analysis without Subgroups
    res <- getHRs.base(data = data,
                       event.time = event.time,
                       event.status = event.status,
                       predictor_formula = predictor_formula,
                       repeatVar = repeatVar)
  }

  return(res)
}

#' Base Function for Hazard Ratio Calculation
#'
#' @param data Data table in data.frame format
#' @param event.time Name, in string format, of column in data containing time-to-event values
#' @param event.status Name, in string format, of column in data containing event status values (as either 0/1 or T/F)
#' @param predictor_formula Arithmetic combination of desired predictor variables in string format. Must use column names in data
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to "FALSE"
#'
#' @return Returns a dataframe of Hazard Ratios and associated confidence intervals and p-values
#'
getHRs.base <- function(data,
                        event.time,
                        event.status,
                        predictor_formula,
                        repeatVar = F) {
  surv <- survival::Surv(data[[event.time]], data[[event.status]])

  f <- formula(paste('surv ~', predictor_formula))

  fit <- survival::coxph(f, data = data, model = T)
  colnames(fit$model) <- event.status
  fit$data <- data

  vars <- getFormulaVars(predictor_formula)
  if(length(vars) > 1) {
    # Create a nested list of baseline and comparison levels for each variable
    vars.groups <- lapply(vars, function(v) {
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

  res <- res[!startsWith(rownames(res),'-'),]

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
