
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

  model_results <- as.data.frame(cbind('OR'=exp(coef(model)),
                                       '2.5%'=exp(confint(model))[,1],
                                       '97.5%'=exp(confint(model))[,2],
                                       'p_value'=(model %>% broom::tidy(exp = T))$p.value))

  model_results <- cbind('Variable'=rownames(model_results), model_results)

  if(!longer) model_results <- cbind('Level'=replicate(nrow(model_results),''),
                                     model_results)

  for(var in names(vars)) {
    if(!is.factor(vars[[var]])) next

    lvls <- levels(vars[[var]])

    r <- grep(var, model_results$Variable)

    lvls.other <- str_replace_all(model_results$Variable[r], var ,'')
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
