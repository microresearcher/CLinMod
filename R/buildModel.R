#' Build a linear regression model
#'
#' @param data Data frame with patient IDs as rows and covariates as columns.
#' @param outcome Outcome variable. Should be the column name of the desired outcome variable in @data.
#' @param predictors Vector of predictor variables to select from when building the model. These should be column names in @data.
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param variable_of_interest (Optional) Variable of interest that from @predictors that must be included in the model.
#' @param include (Optional) Vector of predictor variable(s) listed in @predictors that the function should include first (after @variable_of_interest) when building the model.
#' @param exclude (Optional) Vector of predictor variable(s) listed in @predictors to exclude using when building the model.
#' @param returnAll If "TRUE", returns all models created while iterative building. If "FALSE" (the default), returns the final model created (with the most terms).
#'
#' @return Returns either a list of models of different dimensions with the highest signal-to-noise ratio and fitness for their respective dimensions as determined by AIC and log-likelihood, or returns the highest-dimensional model with the highest signal-to-noise ratio as determined by AIC and log-likelihood.
#' @export
#'
buildModel <- function(data = NA,
                       outcome = NA,
                       predictors = NA,
                       family = 'binomial',
                       variable_of_interest = c(),
                       include = c(),
                       exclude = c(),
                       returnAll = F) {

  if(any(is.na(c(data, outcome, predictors)))) stop('Must enter inputs for "data", "outcome", and "predictors".')

  outcome <- outcome[outcome %in% names(data)]
  predictors <- predictors[predictors %in% names(data)]

  if(!length(outcome)) outcome <- utils::select.list(names(data),
                                              title = 'Please select the outcome variable.')

  if(length(predictors) < 2) predictors <- utils::select.list(names(data), multiple = T,
                                                       title = 'Please select at least 2 predictor variables.')

  variable_of_interest <- variable_of_interest[variable_of_interest %in% predictors]
  include <- include[include %in% predictors]

  cat('\n=== BUILDING MODEL ===\n')

  models <- list()

  predictors.baseline <- union(variable_of_interest, include)
  predictors.added <- c()

  improving <- T
  while(improving) {
    opt_results <- find_best_predictor(data = data,
                                       outcome = outcome,
                                       predictors = predictors,
                                       baseline = predictors.baseline,
                                       include = predictors.added,
                                       exclude = exclude,
                                       verbose = F)

    predictors.baseline <- opt_results$predictors.opt
    predictors.added <- opt_results$newpredictors.best[!(opt_results$newpredictors.best %in% predictors.baseline)]

    models[[paste0(opt_results$predictors.opt, collapse = '_')]] <- opt_results$model.opt

    improving <- opt_results$improving

    if(all(opt_results$newpredictors.best %in% opt_results$predictors.opt)) {
      cat(paste(outcome,'~',
                paste(colnames(opt_results$model.opt$model)[2:ncol(opt_results$model.opt$model)], collapse = ' + ')),
          '\n')
    }
  }

  if(length(models)==1) {
    warning('Only one model could be created. The noise to signal ratio in the data may be too high, leading to poor predictor performance (very few predictors in the model) or overfitting (too many predictors in the model compared to data size).')

    return(models[[1]])
  }

  if(returnAll) return(models)
  else return(models[length(models)])
}

#' Find next best predictor for a model
#'
#' @param data Data frame with patient IDs as rows and covariates as columns.
#' @param outcome Outcome variable. Should be the column name of the desired outcome variable in @data.
#' @param predictors Vector of predictor variables to select from when building the model. These should be column names in @data.
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param baseline (Optional) Predictor variable(s) listed in @predictors to be included in the baseline model to build off of with the rest of the variables listed in @predictors.
#' @param include (Optional) Vector of predictor variable(s) listed in @predictors that the function should include first (after @variable_of_interest) when building the model.
#' @param exclude (Optional) Vector of predictor variable(s) listed in @predictors to exclude using when building the model.
#' @param verbose Whether to print processing statements. Defaults to "TRUE".
#'
#' @return Outputs:
#'    improved (boolean)
#'    model.opt (optimized model)
#'    model.candidate (best model using the variables being tested)
#'    newpredictors.best (best variable)
#'    predictors.opt (set of all predictors used in model.opt)
#'    AICs (list of AICs for all predictors)
#'    LogLiks (list of LogLiks for all predictors)
#' @export
#'
find_best_predictor <- function(data = NA,
                                outcome = NA,
                                predictors = NA,
                                family = 'binomial',
                                baseline = c(),
                                include = c(),
                                exclude = c(),
                                verbose = T) {

  if(any(is.na(c(data, outcome, predictors)))) stop('Must enter a data frame for "data".')

  outcome <- outcome[outcome %in% names(data)]
  predictors <- predictors[predictors %in% names(data)]

  if(!length(outcome)) outcome <- utils::select.list(names(data),
                                              title = 'Please select the outcome variable.')

  if(length(predictors) < 2) predictors <- utils::select.list(names(data), multiple = T,
                                                       title = 'Please select at least 2 predictor variables.')

  baseline <- baseline[baseline %in% predictors]
  include <- include[include %in% predictors]
  exclude <- exclude[exclude %in% predictors]

  if(!length(baseline)) {
    chooseBaseline <- utils::select.list(c('Yes','No'),
                                  title = 'Would you like to select variables for a baseline model?')
    if(chooseBaseline=='Yes') baseline <- utils::select.list(names(data)[!(names(data) %in% outcome)], multiple = T,
                                                      'Please select the variables that the model must include')
  }

  if(length(baseline) > 0) {
    model.baseline <- glm(formula(paste(outcome,'~',paste(baseline, collapse = '+'))),
                          data = data,
                          family = family)
    AIC.baseline <- AIC(model.baseline)
    fitness.baseline <- logLik(model.baseline)

    if(verbose) message('Baseline model:\n  ',paste(outcome,'~',paste(baseline, collapse = ' + ')),
                        '\n  AIC: ', signif(AIC.baseline, digits = 3),
                        '\n  Log Likelihood (goodness of fit): ', signif(fitness.baseline, digits = 3))
  }

  iters <- predictors[!(predictors %in% c(baseline, include, exclude))]

  # Calculating AICs
  AICs <- lapply(iters, function(x) {
    AIC(glm(formula(paste(outcome,'~',paste(c(baseline, include, x),collapse = '+'))),
            data = data,
            family = family))
  })
  names(AICs) <- iters

  # Calculating Log Likelihoods
  fitness <- lapply(iters, function(x) {
    as.numeric(logLik(glm(formula(paste(outcome,'~',paste(c(baseline, include, x),collapse = '+'))),
                          data = data,
                          family = family)))
  })
  names(fitness) <- iters

  # Getting minimum AIC and Log Likelihood
  AIC.min <- AICs[match(min(unlist(AICs)),AICs)]
  fitness.max <- fitness[match(max(unlist(fitness)),fitness)]

  # If new model is improved or there is no baseline, set the output model, else output model is the baseline one
  if((AIC.min < AIC.baseline) | !exists('model.baseline')) {
    model.lowestAIC <- glm(formula(paste(outcome,'~',
                                         paste(c(baseline, include, names(AIC.min)), collapse = ' + '))),
                           data = data,
                           family = family)
  } else model.lowestAIC <- model.baseline

  names(AICs) <- paste0(paste(c(baseline, include),collapse = '_'),'_',names(AICs))
  names(fitness) <- paste0(paste(c(baseline, include),collapse = '_'),'_',names(fitness))

  # If no baseline model, return a model with the best predictor
  if(!exists('model.baseline')) {
    if(verbose) message('\n')
    message(' "',names(AIC.min),'" yielded the lowest AIC of ',signif(unlist(AIC.min), digits = 3))

    predictors.opt <- c(include, names(AIC.min))

    return(list(improving=T,
                model.opt=model.lowestAIC,
                model.candidate=model.lowestAIC,
                newpredictors.best=c(include, names(AIC.min)),
                predictors.opt=predictors.opt,
                AICs=as.data.frame(AICs),
                LogLiks=as.data.frame(fitness)))
  }

  # Compare fitness of new model with baseline model
  if(nrow(model.baseline$model) != nrow(model.lowestAIC$model)) {
    if(fitness.max > fitness.baseline) betterFit <- T
  } else if(AIC.min < AIC.baseline) {
    betterFit <- lmtest::lrtest(model.lowestAIC, model.baseline)$`Pr(>Chisq)`[2] < 0.05
  } else betterFit <- F

  # Messages and model saving depending on fitness test (lrtest)
  if(betterFit) {
    model.opt <- model.lowestAIC
    predictors.opt <- c(baseline, include, names(AIC.min))

    if(verbose) message('\n')
    message(' "',names(AIC.min),'" yielded the lowest AIC of ',signif(unlist(AIC.min), digits = 3),
            ' and provides a better fit than the baseline model.')
    if(verbose) message('\n')

    if(verbose) message('Improved model:\n  ',
                        paste(outcome,'~',paste(c(baseline, include, names(AIC.min)), collapse = ' + ')),
                        '\n  AIC: ', signif(unlist(AIC.min), digits = 3),
                        '\n  Log Likelihood (goodness of fit): ', signif(unlist(fitness.max), digits = 3),'\n')
  } else {
    model.opt <- model.baseline
    predictors.opt <- baseline

    if(verbose) message('\n')
    message(' "',names(AIC.min),'" yielded the lowest AIC of ',signif(unlist(AIC.min), digits = 3),
            ' but did not provide a better fit than the baseline model.')
    if(verbose) message('\n')
  }

  return(list(improving=AIC.min<AIC.baseline,
              model.opt=model.opt,
              model.candidate=model.lowestAIC,
              newpredictors.best=c(include, names(AIC.min)),
              predictors.opt=predictors.opt,
              AICs=as.data.frame(AICs),
              LogLiks=as.data.frame(fitness)))
}
