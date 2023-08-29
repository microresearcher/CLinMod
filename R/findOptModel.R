
#' Title
#'
#' @param data
#' @param outcome
#' @param predictors
#' @param family
#' @param variable_of_interest
#' @param include
#' @param exclude
#' @param dim_ratio
#' @param dim_ratio_lax
#'
#' @return
#' @export
#'
findOptModel <- function(data = NA,
                         outcome = c(),
                         predictors = c(),
                         family = 'binomial',
                         variable_of_interest = c(),
                         include = c(),
                         exclude = c(),
                         dim_ratio = 10,
                         dim_ratio_lax = 1) {

  if(any(is.na(c(data, outcome, predictors)))) stop('Must enter a data frame for "data".')

  outcome <- outcome[outcome %in% names(data)]
  predictors <- predictors[predictors %in% names(data)]

  if(!length(outcome)) outcome <- select.list(names(data),
                                              title = 'Please select the outcome variable.')

  if(length(predictors) < 2) predictors <- select.list(names(data), multiple = T,
                                                       title = 'Please select at least 2 predictor variables.')

  variable_of_interest <- variable_of_interest[variable_of_interest %in% predictors]
  include <- include[include %in% predictors]

  models <- buildModel(data = data,
                       outcome = outcome,
                       predictors = predictors,
                       family = family,
                       variable_of_interest = variable_of_interest,
                       include = include,
                       exclude = exclude,
                       dim_ratio = dim_ratio,
                       dim_ratio_lax = dim_ratio_lax,
                       returnAll = T)

  if(length(models) > 1) model.pruned <- prune_model(models,
                                                     dim_ratio = 10,
                                                     dim_ratio_lax = 1)
  else model.pruned <- models[[1]]

  model.opt <- model.pruned

  cat('\nFinal model:\n  ',
      paste(outcome,'~',paste(colnames(model.opt$model)[2:ncol(model.opt$model)], collapse = ' + ')),
      '\n\n')

  return(model.opt)
}
