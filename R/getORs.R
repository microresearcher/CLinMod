#' Wrapper function for getting Odds Ratios
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm" to calculate OR from a linear model. Must either provide a model or @data with @variable and @response of interest
#' @param data Data table in data.frame format to calculate OR directly from the data using @variable and @response arguments
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param alpha Significance level. Defaults to 0.05
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to "FALSE".
#'
#' @return Returns table of ORs
#' @export
#'
getORs <- function(model = NULL,
                   data = NULL, variable = NULL, response = NULL,
                   alpha = 0.05,
                   longer = F, repeatVar = F) {
  if(length(model)) res <- getORs.LM(model = model, longer = longer, repeatVar = repeatVar)
  else if(all(length(data), length(variable), length(response))) {
    res <- getORs.LM(data = data, variable = NULL, response = NULL,
                     alpha = alpha,
                     longer = longer, repeatVar = repeatVar)
  } else stop('Must provide either a model or alternatively data, variable, and response')

  return(res)
}

#' Get Odds Ratios with confidence intervals and p-values from a linear model
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to "FALSE".
#'
#' @return Dataframe of odds ratios for each predictor in the model.
#' @export
#'
getORs.LM <- function(model, longer=F, repeatVar=F) {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  vars <- model$model[0,]
  vars <- vars[2:ncol(vars)]

  model_results <- as.data.frame(cbind('OR'=exp(stats::coef(model)),
                                       'CI.lower'=exp(stats::confint(model))[,1],
                                       'CI.upper'=exp(stats::confint(model))[,2],
                                       'p'=(model %>% broom::tidy(exp = T))$p.value))

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

    lvls.other <- stringr::str_replace_all(model_results$Variable[r], var ,'')
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
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param response Name of column, in string format, containing boolean response values (as either 0/1 or T/F)
#' @param alpha Significance level. Defaults to 0.05
#'
#' @return Dataframe of Odds Ratio and associated confidence interval and p-value
#' @export
#'
getOR.manual <- function(data,
                         variable,
                         response,
                         alpha=0.05) {
  variable <- variable[variable %in% colnames(data)]
  response <- response[response %in% colnames(data)]

  freqtab <- table(data[[variable]],data[[response]])

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
