#' Get Odds Ratios with confidence intervals and p-values from a linear model
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param longer Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").
#' @param repeatVar Whether to repeat the variable name on the left-most column next to each category for categorical variables. Defaults to "FALSE".
#'
#' @return Dataframe of odds ratios for each predictor in the model.
#' @export
#'
getORs <- function(model, repeatVar=F, longer=F) {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  vars <- model$model[0,]
  vars <- vars[2:ncol(vars)]

  model_results <- as.data.frame(cbind('OR'=exp(stats::coef(model)),
                                       '2.5%'=exp(stats::confint(model))[,1],
                                       '97.5%'=exp(stats::confint(model))[,2],
                                       'p_value'=(model %>% broom::tidy(exp = T))$p.value))

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

  return(model_results)
}

#' Get the Odds Ratio for a single variable directly from the data
#'
#' @param data Data table in data.frame format
#' @param variable Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values
#' @param outcome Name of column, in string format, containing outcome values (as either 0/1 or T/F)
#' @param alpha Significance level. Defaults to 0.05
#'
#' @return Dataframe of Odds Ratio and associated confidence interval and p-value
#' @export
#'
getOR.manual <- function(data,
                         variable,
                         outcome,
                         alpha=0.05) {
  variable <- variable[variable %in% colnames(data)]
  outcome <- outcome[outcome %in% colnames(data)]

  freqtab <- table(data[[variable]],data[[outcome]])

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
  # OR_CI <- c(OR_CI.lower,OR_CI.upper)

  res <- data.frame(OR,OR_CI.lower,OR_CI.upper,p)
  colnames(res) <- c('OR',
                     paste0(alpha/2*100,'% CI'),
                     paste0((1-alpha/2)*100,'% CI'),
                     'p')

  return(res)
}
