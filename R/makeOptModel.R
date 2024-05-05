
#' Find an optimized linear model for an response variable from a list of predictors
#'
#' @param data Data frame with patient IDs as rows and covariates as columns.
#' @param response Response variable. Should be the column name of the desired response variable in @data.
#' @param time For survival data, the time variable corresponding to the response.
#' @param predictors Vector of predictor/independent variables to select from when building the model. These should be column names in @data.
#' @param include (Optional) Vector of predictor variable(s) listed in @predictors that must be included in the model.
#' @param exclude (Optional) Vector of predictor variable(s) listed in @predictors to exclude when building the model.
#' @param direction The mode of stepwise model creation. Can be one of "build", "prune", or "both". Defaults to "both".
#' @param limitDim Whether to limit the dimensionality (size) of the model based on the number of observations in the data.
#' @param dim_ratio Number used in the pruning step to determine the highest allowed dimensionality of the model (how many covariates the model is allowed to have on the right-hand side). The number of events in the data divided by @dim_ratio (rounded down to the nearest whole number) yields the highest dimensionality allowed. Defaults to 10.
#' @param dim_ratio_lax How many more dimensions is the model allowed to have than that calculated using @dim_ratio_lax. For example, for a dataset with 26 events and a @dim_ratio of 10, 26/10 = 2.6. A @dim_ratio_lax of 0 in this case would prune a model to 2 dimensions, while a @dim_ratio_lax of 1 would prune the model to 3 dimensions (assuming a model with at least 3 dimensions was able to be built).
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param trace Whether to print out all AICs at each step. Defaults to TRUE.
#'
#' @return Returns a model from the @data with the specified @response variable and @variable_of_interest, trying to include as many of the @predictors specified in @include, and excluding any @predictors specified in @exclude, pruned using @dim_ratio and @dim_ratio_lax as explained above.
#' @export
#'
makeOptModel <- function(data, response, time = NULL, predictors = c(),
                         include = c(), exclude = c(),
                         direction = c('both','prune','build'),
                         limitDim = T, dim_ratio = 10, dim_ratio_lax = 0,
                         family = 'binomial',
                         trace = T) {
  # If no valid response variable was specified, ask user to select it from dataframe columns
  response <- intersect(response, colnames(data))
  if(!length(response)) response <- utils::select.list(colnames(data),
                                                     title = 'Please select the response variable.')

  # If value was given for "time" then exit with error if any of the below are true:
  #  Length of value entered for "time" or "response are >1
  #  If value assigned to time is not found in column names of data
  #  If the response column is not a vector of 0's and 1's
  if(length(time)) {
    # Check the "time" input
    if(length(time) > 1) stop('"time" must be a single column name in the data.')
    if(!(time %in% colnames(data))) stop('"',time,'" must be a single column name in the data.')
    # Check the "response" input
    if(length(response) > 1) stop('"response" must be a single column name in the data for survival data.')
    # Get all values in "response" that are not 0 or 1, or missing
    na_response <- setdiff(data[[response]], c(0, 1, NA))
    if(length(na_response)) stop('"response" values must be 0 or 1 for "event" and "censored", respectively.\n  The following additional values were found and these samples/cases should be removed from the data:\n   ',
                                paste(na_response, collapse = '\n   '))
  }

  # If at least 2 valid predictors were not specified, ask user to choose from dataframe columns
  predictors <- intersect(predictors, colnames(data))
  while(length(predictors) < 2) predictors <- utils::select.list(setdiff(colnames(data), c(response, time)),
                                                                 multiple = T,
                                                                 title = 'Please select at least 2 potential predictor variables.')

  include <- intersect(include, predictors)
  # # If there are no valid variables in "include", it will be intercept
  # if(!length(include)) include <- 1

  # Notify user with a warning if any names in "include" were not found in dataframe column names
  if(any(!(include %in% predictors))) warning('The following variable names were not found in the dataframe columns and cannot be included in the model:\n  ',
                                              paste(setdiff(include, predictors), collapse = '\n  '),'\n')

  # Reduce data to only the rows with complete cases when all possible variables are included (except those in "exclude")
  data.reduced <- na.omit(data[c(time, response, setdiff(predictors, exclude))])
  # Create "surv" object with the reduced data if there was a valid "time" value
  if(length(time)) surv <- Surv(as.numeric(data.reduced[[time]]),
                                as.numeric(data.reduced[[response]]))

  direction <- match.arg(direction, c('both','prune','build'))
  models.opt <- list()
  # Start with largest possible model and prune it
  if(direction != 'build') {
    cat('\n/// ====== PRUNING LARGEST MODEL ====== \\\\\\\ \n')
    model.largest <- makeModel(data = data.reduced,
                               response = response,
                               time = time,
                               predictors = predictors,
                               family = family)

    models.opt$pruned <- optModel.AIC(model = model.largest,
                                      predictors = predictors,
                                      keep = include,
                                      direction = direction,
                                      limitDim = limitDim,
                                      dim_ratio = dim_ratio,
                                      dim_ratio_lax = dim_ratio_lax,
                                      verbose = F,
                                      trace = trace)
  }
  # Start with simplest possible model and build on it
  if(direction != 'prune') {
    cat('\n/// ====== BUILDING FROM SMALLEST MODEL ====== \\\\\\\ \n')
    # If there are no valid variables in "include", it will be intercept
    model.smallest <- makeModel(data = data.reduced,
                                response = response,
                                time = time,
                                predictors = (if(length(include)) include
                                              else 1),
                                family = family)

    models.opt$built <- optModel.AIC(model = model.smallest,
                                     predictors = predictors,
                                     keep = include,
                                     direction = direction,
                                     limitDim = limitDim,
                                     dim_ratio = dim_ratio,
                                     dim_ratio_lax = dim_ratio_lax,
                                     verbose = F,
                                     trace = trace)
  }

  if(length(models.opt$pruned)) cat('\nOptimally Pruned Model:\n ',
                                    paste(models.opt$pruned$formula[2]),
                                    paste(models.opt$pruned$formula[1]),
                                    paste(models.opt$pruned$formula[3]),
                                    '\n  AIC:', extractAIC(models.opt$pruned)[2],'\n')

  if(length(models.opt$built)) cat('\nOptimally Built Model:\n ',
                                   paste(models.opt$built$formula[2]),
                                   paste(models.opt$built$formula[1]),
                                   paste(models.opt$built$formula[3]),
                                   '\n  AIC:', extractAIC(models.opt$built)[2],'\n')

  ### Now... what to do with model.pruned and model.built?

  model.opt <- models.opt[[order(sapply(models.opt,
                                        function (m) extractAIC(m)[2]))[1]]]

  cat('\n<<< === FINAL MODEL === >>>\n  ',
      paste(model.opt$formula[2]),
      paste(model.opt$formula[1]),
      paste(model.opt$formula[3]),
      '\n\n')

  return(model.opt)
}

#' Optimize a Model using AIC
#'
#' @param model Model of class "aov", "lm", "glm", "mlm", "coxph", and potentially other models. Passed to stats::update() through wrapper updateModel().
#' @param predictors Vector of all predictor/independent variables to consider including in the model. These should be column names in @model$data.
#' @param keep (Optional) Vector of predictor variable(s) listed in @predictors that must be included in the model.
#' @param direction The mode of stepwise model creation. Can be one of "build", "prune", or "both". Defaults to "both".
#' @param limitDim Whether to limit the dimensionality (size) of the model based on the number of observations in the data.
#' @param dim_ratio Number used in the pruning step to determine the highest allowed dimensionality of the model (how many covariates the model is allowed to have on the right-hand side). The number of events in the data divided by @dim_ratio (rounded down to the nearest whole number) yields the highest dimensionality allowed. Defaults to 10.
#' @param dim_ratio_lax How many more dimensions is the model allowed to have than that calculated using @dim_ratio_lax. For example, for a dataset with 26 events and a @dim_ratio of 10, 26/10 = 2.6. A @dim_ratio_lax of 0 in this case would prune a model to 2 dimensions, while a @dim_ratio_lax of 1 would prune the model to 3 dimensions (assuming a model with at least 3 dimensions was able to be built).
#' @param verbose Whether to print out all messages such as final model. Defaults to TRUE. Details of each optimization step will always be printed out.
#' @param trace Whether to print out all AICs at each step. Defaults to TRUE.
#'
#' @return Returns an optimized model informed by stepwise AIC minimization.
#' @export
#'
optModel.AIC <- function(model, predictors, keep = c(),
                         direction = c('both','prune','build'),
                         limitDim = T, dim_ratio = 10, dim_ratio_lax = 0,
                         verbose = T, trace = T) {
  # Check if "model" is a valid model by running it through the "update" function
  updateModel(model, . ~ .)

  # Identify the response variable of the model
  response <- colnames(model$model)[1]

  # Identify predictors that are already in the model and those that can be added
  #  Make sure to only try to look for column names in the model if it has more than 1 column
  #  If given a model with only an intercept and no predictors, the model will have only 1 column which pertains to the response variable
  predictors <- unique(c(intersect(keep, predictors),
                         if(ncol(model$model) > 1) colnames(model$model)[2:ncol(model$model)],
                         intersect(predictors, colnames(model$data))))

  # If at least 2 valid predictors are not in the model or otherwise specified, ask user to choose from columns in model$data
  #  If insufficient columns in the model's data, then nothing to do and function will exit with error
  while(length(predictors) < 2 & ncol(model$model) > 2) {
    predictors <- utils::select.list(colnames(model$data)[2:ncol(model$model)],
                                     multiple = T,
                                     title = 'Please select at least 2 potential predictor variables.')
  }
  if(length(predictors) < 2) stop('No model optimization needed if less than 2 possible predictor variables.')

  # Notify user with a warning if any names in "keep" were not found in dataframe column names
  if(length(setdiff(keep, c(predictors, 1)))) {
    warning('The following variable names were not found in the data column names and cannot be included in the model:\n   ',
            paste(setdiff(keep, predictors),
                  collapse = '\n  '),'\n')
  }
  keep <- intersect(keep, predictors)
  # If there are more variables that must be in the model than would be allowed by dimensionality restraints, set limitDim to FALSE
  if(length(keep) > floor(nobs(model)/dim_ratio) + dim_ratio_lax) {
    limitDim <- F
  }

  direction <- match.arg(direction, c('both','prune','build'))
  # If direction is "both" or "prune", make a "drop" list for predictors that are in the model and can be dropped
  if(direction != 'build') {
    predictors.drop <- intersect(predictors, colnames(model$model))
  } else predictors.drop <- NULL
  # If direction is "both" or "build", make an "add" list for the predictors not currently in the model that can be added
  if(direction != 'prune') {
    predictors.add <- setdiff(predictors, colnames(model$model))
  } else predictors.add <- NULL

  # Prepend predictor names in the "drop" and "add" lists with "-" or "+" respectively
  if(length(predictors.drop)) predictors.drop <- paste('-',predictors.drop)
  if(length(predictors.add)) predictors.add <- paste('+',predictors.add)

  if(!all(complete.cases(model$data[c(response, predictors)]))) {
    warning('Provided model was built using data with some missing values.\n  Data will be limited to only the cases which are complete (no missing values) for all possible predictors.')
    model$data <- model$data[complete.cases(model$data[c(response, predictors)]),]
  }

  cat('\nCurrent Model:\n',
      paste(model$formula[2]),paste(model$formula[1]),paste(model$formula[3]),
      '\n  AIC:', extractAIC(model)[2],
      '\n  Number of events/samples:', nobs(model),'\n\n')

  improving <- T
  overfitted <- F # Only evaluate for overfitting risk if limitDim is TRUE (evaluated in loop)
  while(improving | overfitted) {
    # If trace is TRUE, then this helps make output easier to read.
    if(trace) cat('<> Next Step <>\n')
    AICs.drop <- sapply(predictors.drop, function(p) {
      extractAIC(updateModel(model, formula(paste('.~.',p))))[2]
    })
    AICs.add <- sapply(predictors.add, function(p) {
      extractAIC(updateModel(model, formula(paste('.~.',p))))[2]
    })

    # Named vector of all AIC options
    AICs <- unlist(c(AICs.drop, AICs.add, '<current>' = extractAIC(model)[2]))
    # If still iterating but model is no longer improving, then function is trying to remove a variable to meet dimensionality restrictions
    #  In this case, only consider AICs that involve removing a variable
    if(!improving) AICs <- AICs[startsWith(names(AICs),'-')]
    # Reorder the AICs from least to greatest
    AICs <- AICs[order(AICs, decreasing = F)]
    # Named value of lowest AIC even if it corresponds to removing a variable in the "keep" list
    AIC.min <- AICs[1]
    # Named value of lowest AIC that does not correspond to removing a variable in the "keep" list
    AIC.next <- AICs[!(names(AICs) %in% paste('-',keep))][1]

    # If lowest AIC would result from removing a variable in "keep", then let the user know and use the next best that is not in "keep"
    #  This will not let user know if the second lowest AIC also results from removing a variable in "keep".
    #  User will be alerted to this and can see the trace if printed.
    if(!identical(AIC.min, AIC.next)) message('Removing ', gsub('\\- ','',names(AIC.min)),
                                              ' yields a lower AIC than the current model but will be kept since it must be included in the model.')
    if(trace) print(data.frame(AICs))

    if(names(AIC.next) == '<current>') {
      cat('<> Lowest AIC of',AICs[which(names(AICs)=='<current>')+1],
          'with',paste0('"',names(AICs[which(names(AICs)=='<current>')+1]),'"'),
          'is still greater than the current',paste0(round(AICs['<current>'], 4),
                                                     '.\n Thus, optimization has reached a local minimum.\n'))
      if(verbose) cat('Current Model:\n ',
                      paste(model$formula[2]),paste(model$formula[1]),paste(model$formula[3]),
                      '\n  AIC:', extractAIC(model)[2],'\n')
      improving <- F
    } else {
      cat(paste0('<> "',names(AIC.next),'" yields a lower AIC than the current model.'))
      model <- updateModel(model, formula(paste('.~.',names(AIC.next))))
      # Remove the predictor from the list it was in and put it in the other list
      #  If direction is "build" leave the "drop" list NULL
      if(direction != 'build') {
        predictors.drop <- unique(c(setdiff(predictors.drop, names(AIC.next)),
                                    gsub('\\+ ','\\- ',
                                         names(AIC.next)[substr(names(AIC.next), 1, 1)=='+'])))
      }
      #  If direction is "prune" leave the "add" list NULL
      if(direction != 'prune') {
        predictors.add <- unique(c(setdiff(predictors.add, names(AIC.next)),
                                   gsub('\\- ','\\+ ',
                                        names(AIC.next)[substr(names(AIC.next), 1, 1)=='-'])))
      }
      cat('\nNew Model:\n',
          paste(model$formula[2]),paste(model$formula[1]),paste(model$formula[3]),
          '\n  AIC:', extractAIC(model)[2],'\n')
    }

    # If model dimensionality is to be limited by sample/event size, then calculate whether there is high risk of overfitting
    if(limitDim) {
      # I need a new function for nobs when event size is desired over sample size for non-survival data
      n <- nobs(model)

      n_vars <- length(paste(getFormulaVars(model$formula)[-1]))
      overfitted <- n_vars > floor(n / dim_ratio) + dim_ratio_lax
      cat(' ',n_vars,'of',floor(n / dim_ratio) + dim_ratio_lax,'allowed predictors for data with',n,'samples/events.\n')
      if(overfitted & !improving) message('   This does not meet the dimensionality constraints set by the user.\n   Pruning will continue until there are at most ',
                                          paste(floor(n/dim_ratio) + dim_ratio_lax),' variables.')
    }
    cat('\n')
  }

  cat('\\\\\\ ====== OPTIMIZATION COMPLETE ====== ///\n')

  return(model)
}
