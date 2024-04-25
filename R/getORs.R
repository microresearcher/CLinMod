#' Wrapper function for getting Odds Ratios
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm" to calculate OR from a linear model. Must either provide a model or @data with @variable and @response of interest
#' @param data Data table in data.frame format to calculate OR directly from the data using @variable and @response arguments
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param predictors Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values or continuous values
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param subgroup_by (Optional) Vector of variable names for subgroup analyses. Must be column names in data that are not in predictor_formula.
#' @param alpha Significance level. Defaults to 0.05
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical predictors. Defaults to "FALSE".
#' @param verbose Whether to print extra information during processing and analysis. Defaults to True.
#'
#' @return Returns table of ORs
#' @export
#'
getORs <- function(model = NULL,
                   data = NULL, response = NULL, predictors = NULL, family = 'binomial',
                   subgroup_by = NULL,
                   alpha = 0.05,
                   longer = F, repeatVar = F,
                   verbose = T) {
  if(length(model)) {
    cat('\nUsing provided model to calculate ORs.\n\n')
    if(length(subgroup_by)) warning(' Ignoring subgroup_by since a model is already provided.')
    subgroups <- NULL # Cannot subgroup if a model is already provided
  } else if(all(length(data), length(predictors), length(response))) {
    response <- response[response %in% colnames(data)]
    vars <- predictors[predictors %in% colnames(data)]
    subgroups <- intersect(subgroup_by, colnames(data))

    if(length(intersect(subgroups, vars))) stop('Stratification variable cannot be in the predictor formula.')

    if(!length(subgroups)) {
      cat('\nGenerating model of', response,
          'using', paste0(paste(vars, collapse = ', '),'.'),
          '\n\n')

      data.reduced <- na.omit(data[c(response, vars)])

      model <- glm(formula(paste(response,'~',
                                 paste(vars, collapse = '+'))),
                   data = data.reduced,
                   family = family)
    }
  } else stop('Must provide either a model or alternatively data, variable, and response')

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

      grouped <- data[complete.cases(data[c(response, vars, subgrp)]),] %>%
        dplyr::group_by(.data[[subgrp]])

      res.grouped <- grouped %>%
        dplyr::group_map(~ getORs.base(glm(formula(paste(response,'~',
                                                         paste(vars, collapse = ' + '))),
                                           data = .x,
                                           family = family),
                                       longer = longer,
                                       repeatVar = T))

      if(verbose) print(subgrps.n)

      names(res.grouped) <- (grouped %>% dplyr::group_keys())[[1]]

      res.sub <- dplyr::bind_rows(lapply(names(res.grouped)[!is.na(names(res.grouped))],
                                         function(grp) cbind(Lvl = grp, Var = subgrp, res.grouped[[grp]])))

      res.sub <- res.sub[res.sub$Variable != '(Intercept)' & complete.cases(res.sub),]

      inf_grps <- apply(res.sub[c('Odds Ratio', 'CI lower', 'CI upper', 'p-value')],
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

    res <- dplyr::bind_rows(res.subgrps)
  } else res <- getORs.base(model = model,
                            longer = longer,
                            repeatVar = repeatVar)

  cat('\n')

  return(res)
}

#' Get Odds Ratios with confidence intervals and p-values from a linear model
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical predictors. Defaults to "FALSE".
#'
#' @return Dataframe of odds ratios for each predictor in the model.
#' @export
#'
getORs.base <- function(model, longer=F, repeatVar=F) {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  vars <- model$model[0,]
  vars <- vars[2:ncol(vars)]

  model_results <- as.data.frame(cbind('OR'=exp(stats::coef(model)),
                                       'CI.lower'=exp(stats::confint(model))[,1],
                                       'CI.upper'=exp(stats::confint(model))[,2],
                                       'p'=(model %>% broom::tidy(exp = T))$p.value
                                       ))

  # If none of the predictors have more than one level, use the better p-value
  if(ncol(vars) == nrow(model_results) - 1) {
    model_results$p <- stats::anova(model, test = 'Chisq')$`Pr(>Chi)`
  }

  colnames(model_results) <- c('Odds Ratio',
                               'CI lower',
                               'CI upper',
                               'p-value')

  model_results <- cbind('Variable'=rownames(model_results), model_results)

  if(!longer) model_results <- cbind('Level'=replicate(nrow(model_results),''),
                                     model_results)

  for(var in names(vars)) {
    if(!is.factor(vars[[var]])) next

    lvls <- levels(vars[[var]])

    r <- grep(var, model_results$Variable)

    lvls.other <- stringr::str_replace(model_results$Variable[r], var ,'')
    lvl.baseline <- lvls[!(lvls %in% lvls.other)]

    if(repeatVar) model_results$Variable[r] <- var
    else model_results$Variable[r] <- ''
    model_results[r,1] <- lvls.other

    model_results <- rbind(model_results[1:(min(r)-1),],
                           rep('',ncol(model_results)),
                           model_results[min(r):nrow(model_results),])

    model_results[min(r),1] <- lvl.baseline
    model_results$Variable[min(r)] <- var

    if(longer) {
      model_results[r+1,1] <- paste0('   ', model_results[r+1,1])

      model_results <- rbind(model_results[1:min(r),],
                             rep('',ncol(model_results)),
                             model_results[min(r+1):nrow(model_results),])

      model_results[min(r+1),1] <- paste0('   ', lvl.baseline)
    }
  }

  model_results <- cbind('Variable'=model_results$Variable,
                         model_results[colnames(model_results)!='Variable'])

  rownames(model_results) <- 1:nrow(model_results)

  model_results[c('Odds Ratio','CI lower','CI upper','p-value')] <- sapply(model_results[c('Odds Ratio',
                                                                                           'CI lower',
                                                                                           'CI upper',
                                                                                           'p-value')],
                                                                           function(x) suppressWarnings(as.numeric(x)))

  return(model_results)
}

#' Get the Odds Ratio for a single binary variable directly from the data
#'
#' @param data Data table in data.frame format
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param alpha Significance level. Defaults to 0.05
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical predictors. Defaults to "FALSE".
#'
#' @return Dataframe of Odds Ratio and associated confidence interval and p-value
#' @export
#'
getOR.manual <- function(data,
                         response,
                         variable,
                         alpha=0.05,
                         longer = longer, repeatVar=F) {
  variable <- variable[variable %in% colnames(data)]
  response <- response[response %in% colnames(data)]

  freqtab <- table(data[[variable]],
                   data[[response]])

  a <- freqtab[1,1]
  b <- freqtab[1,2]
  c <- freqtab[2,1]
  d <- freqtab[2,2]

  OR <- as.numeric((d/c)/(b/a))
  p <- (factorial(a+b)*factorial(a+c)*factorial(b+d)*factorial(c+d))/
    (factorial(a+b+c+d)*factorial(a)*factorial(b)*factorial(c)*factorial(d))

  logSE <- sqrt(1/a+1/b+1/c+1/d)

  z <- abs(qnorm(alpha/2))
  OR_CI.lower <- exp(log(OR) - z*logSE)
  OR_CI.upper <- exp(log(OR) + z*logSE)

  res <- data.frame(OR,OR_CI.lower,OR_CI.upper,p)
  colnames(res) <- c('OR',
                     paste0(alpha/2*100,'% CI'),
                     paste0((1-alpha/2)*100,'% CI'),
                     'p')

  return(res)
}
