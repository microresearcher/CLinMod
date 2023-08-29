#' Prune a model from a list of models provided
#'
#' @param models
#' @param dim_ratio
#' @param dim_ratio_lax
#'
#' @return
#' @export
#'
pruneModel <- function(models,
                       dim_ratio = 10,
                       dim_ratio_lax = 1) {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(models) %in% valid_classes) &
     !all(sapply(models, function(x) any(class(x) %in% valid_classes)))) {
    stop(cat(message('Models must be a model or list of models of one of the following classes:\n  ',
                     paste(valid_classes, collapse = '\n  '),'\n')))
  }

  if(any(class(models) %in% valid_classes)) {
    model.simplest <- glm(data = models$data,
                          formula = formula(paste(colnames(models$model)[1],'~',colnames(models$model)[2])),
                          family = 'binomial')

    models <- list(model.simplest,
                   models)
  }

  if(length(models) < 2) stop('\nNeed a list of more than 1 model to develop a pruned model. If only one model was able to be created then there is likely not a pruned model that exists that fits the data sufficiently well. Data is likely too sparse')

  cat('\n=== PRUNING MODEL ===\n')

  model.temp <- models[[length(models)]]

  n_events <- summary(model.temp$model[[1]])[2]
  predictors.temp <- colnames(model.temp$model)[2:ncol(model.temp$model)]

  cat(length(predictors.temp),' predictors for data with ',n_events,' events.\n')

  model.temp <- models[[length(models)]]

  models.predictors <- lapply(models, function(x) colnames(x$model[2:ncol(x$model)]))

  # this used to be "while(abs(length(predictors.temp) - n_events/dim_ratio) > dim_ratio_lax)", not sure why
  while(length(predictors.temp) - n_events/dim_ratio > dim_ratio_lax) {
    n_predictors.diff <- sapply(models.predictors, function(x) length(setdiff(predictors.temp, x)))
    n <- min(grep(0, n_predictors.diff))

    if(n < 2) break

    predictors.added <- predictors.temp[!(predictors.temp %in% colnames(models[[n-1]]$model))]
    # If current model is one of the models generated above, then don't try to prune the last predictor added to the model
    if(length(predictors.temp)==length(models.predictors[[n]])) {
      predictors.added <- predictors.added[-length(predictors.added)]
    }

    if(length(predictors.added) > 1) {
      AICs.pruned <- sapply(predictors.added, function(x) AIC(change_model(model.temp, remove = x)))
      predictor.prune <- names(AICs.pruned)[match(min(AICs.pruned), AICs.pruned)]

      model.temp <- change_model(model.temp, remove = predictor.prune)
    } else {
      predictor.prune <- predictors.temp[!(predictors.temp %in% colnames(models[[n-1]]$model))]
      model.temp <- models[[n-1]]
    }

    predictors.temp <- colnames(model.temp$model)[2:ncol(model.temp$model)]

    message(' "',predictor.prune,'" was pruned based on the AIC of the resulting model.')
  }

  cat('\nNo further pruning needed.\n')

  model.pruned <- model.temp

  return(model.pruned)
}
