#' Prune a model from a list of models provided
#'
#' @param models List of models for a dataset with differing dimensions. Models must be of class "aov", "lm", "glm", or "mlm". This list does not need to have models of all dimensionalities from 1 to the highest dimensionality.
#' @param keep (Optional) Variable of interest that from @predictors that must be included in the model.
#' @param dim_ratio Number used to determine the highest allowed dimensionality of the model (how many covariates the model is allowed to have on the right-hand side). The number of events in the data divided by @dim_ratio (rounded down to the nearest whole number) yields the highest dimensionality allowed. Defaults to 10.
#' @param dim_ratio_lax How many more dimensions is the model allowed to have than that calculated using @dim_ratio. For example, for a dataset with 36 events and a @dim_ratio of 10, floor(36/10) = 3. A @dim_ratio_lax of 0 in this case would prune a model to 3 dimensions, while a @dim_ratio_lax of 1 would prune the model to 4 dimensions (assuming a model with at least 4 dimensions was able to be built).
#' @param verbose Whether to print messages about starting and final models.
#'
#' @return Pruned model based on number of events in the dataset used to build the @models, using the highest dimensionality model provided, with the other models used as benchmarks at their respective dimensionalities.
#' @export
#'
pruneModel <- function(models,
                       keep = NULL,
                       dim_ratio = 10,
                       dim_ratio_lax = 1,
                       verbose = T) {
  valid_classes <- c('aov','lm','glm','mlm')

  # Error if model is not one of, or a list of, objects of the classes above
  if(!any(class(models) %in% valid_classes) &
     !all(sapply(models, function(x) any(class(x) %in% valid_classes)))) {
    stop(cat(message('Models must be a model or list of models of one of the following classes:\n  ',
                     paste(valid_classes, collapse = '\n  '),'\n')))
  }

  # If only one model was given, then ensure that the simplest possible model, containing at least 1 variable, is an option.
  if(any(class(models) %in% valid_classes)) {
    keep <- keep[keep %in% colnames(models$model)[2:ncol(models$model)]]
    if(!is.null(keep)) {
      model.simplest <- stats::glm(data = models$data,
                                   formula = stats::formula(paste(colnames(models$model)[1],'~',
                                                                  paste(keep, collapse = '+'))),
                                   family = 'binomial')
    } else {
      model.simplest <- stats::glm(data = models$data,
                                   formula = stats::formula(paste(colnames(models$model)[1],'~',colnames(models$model)[2])),
                                   family = 'binomial')
    }

    models <- list(model.simplest,
                   models)
  } else {
    keep <- keep[keep %in% colnames(models[[1]]$model)[2:ncol(models[[1]]$model)]]
  }

  # if(length(models) < 2) stop('\nNeed a list of more than 1 model to develop a pruned model. If only one model was able to be created then there is likely not a pruned model that exists that fits the data sufficiently well. Data is likely too sparse.')

  cat('\n=== PRUNING MODEL ===\n')

  model.temp <- models[[length(models)]]

  if(verbose) cat('\nStarting model:\n',
                  paste(colnames(model.temp$model)[1]),'~',
                  paste(colnames(model.temp$model)[2:ncol(model.temp$model)], collapse = ' + '),
                  '\n AIC:', AIC(model.temp),
                  '\n')

  n_events <- summary(model.temp$model[[1]])[2]
  predictors.temp <- colnames(model.temp$model)[2:ncol(model.temp$model)]

  cat('',length(predictors.temp),' predictors for data with ',n_events,' events.\n')

  models.predictors <- lapply(models, function(x) colnames(x$model[2:ncol(x$model)]))

  while(length(predictors.temp) - floor(n_events/dim_ratio) > dim_ratio_lax) {
    n_predictors.diff <- sapply(models.predictors, function(x) length(setdiff(predictors.temp, x)))
    n <- min(which(n_predictors.diff==0))

    if(n < 2) break

    predictors.added <- predictors.temp[!(predictors.temp %in% colnames(models[[n-1]]$model))]
    # If current model is one of the models generated above, then don't try to prune the last predictor added to the model
    if(length(predictors.temp)==length(models.predictors[[n]])) {
      predictors.added <- predictors.added[-length(predictors.added)]
    }

    if(length(predictors.added) > 1) {
      AICs.pruned <- sapply(predictors.added, function(x) AIC(changeModel(model.temp, drop = x)))
      predictor.prune <- names(AICs.pruned)[match(min(AICs.pruned), AICs.pruned)]

      # if(verbose) print(AICs.pruned[order(AICs.pruned)])

      model.temp <- changeModel(model.temp, drop = predictor.prune)
    } else {
      predictor.prune <- predictors.temp[!(predictors.temp %in% colnames(models[[n-1]]$model))]
      model.temp <- models[[n-1]]
    }

    predictors.temp <- colnames(model.temp$model)[2:ncol(model.temp$model)]

    message(' "',predictor.prune,'" was pruned based on the AIC of the resulting model: ',
            signif(AICs.pruned[predictor.prune], 4),'.\n')

    cat(paste(colnames(model.temp$model)[1]),'~',
        paste(colnames(model.temp$model)[2:ncol(model.temp$model)], collapse = ' + '),
        '\n AIC:', AIC(model.temp),
        '\n')
    cat('',length(predictors.temp),' predictors for data with ',n_events,' events.\n')
  }

  cat('\n=== PRUNING COMPLETE ===\n')

  model.pruned <- model.temp

  if(verbose) cat('\nFinal model:\n  ',
                  paste(colnames(model.temp$model)[1]),'~',
                  paste(colnames(model.temp$model)[2:ncol(model.temp$model)], collapse = ' + '),
                  '\n')

  return(model.pruned)
}
