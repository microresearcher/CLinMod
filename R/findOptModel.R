
#' Find an optimized linear model for an outcome variable from a list of predictors
#'
#' @param data Data frame with patient IDs as rows and covariates as columns.
#' @param outcome Outcome variable. Should be the column name of the desired outcome variable in @data.
#' @param predictors Vector of predictor variables to select from when building the model. These should be column names in @data.
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#' @param variable_of_interest (Optional) Variable of interest that from @predictors that must be included in the model.
#' @param include (Optional) Vector of predictor variable(s) listed in @predictors that the function should include first (after @variable_of_interest) when building the model.
#' @param exclude (Optional) Vector of predictor variable(s) listed in @predictors to exclude using when building the model.
#' @param dim_ratio Number used in the pruning step to determine the highest allowed dimensionality of the model (how many covariates the model is allowed to have on the right-hand side). The number of events in the data divided by @dim_ratio (rounded down to the nearest whole number) yields the highest dimensionality allowed. Defaults to 10.
#' @param dim_ratio_lax How many more dimensions is the model allowed to have than that calculated using @dim_ratio_lax. For example, for a dataset with 26 events and a @dim_ratio of 10, 26/10 = 2.6. A @dim_ratio_lax of 0 in this case would prune a model to 2 dimensions, while a @dim_ratio_lax of 1 would prune the model to 3 dimensions (assuming a model with at least 3 dimensions was able to be built).
#'
#' @return Returns a model from the @data with the specified @outcome variable and @variable_of_interest, trying to include as many of the @predictors specified in @include, and excluding any @predictors specified in @exclude, pruned using @dim_ratio and @dim_ratio_lax as explained above.
#' @export
#'
findOptLM <- function(data = NA,
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

  if(!length(outcome)) outcome <- utils::select.list(names(data),
                                                     title = 'Please select the outcome variable.')

  if(length(predictors) < 2) predictors <- utils::select.list(names(data), multiple = T,
                                                              title = 'Please select at least 2 potential predictor variables.')

  variable_of_interest <- variable_of_interest[variable_of_interest %in% predictors]
  include <- include[include %in% predictors]

  # Build model informed by AIC and fitness
  models <- buildLM(data = data,
                    outcome = outcome,
                    predictors = predictors,
                    family = family,
                    variable_of_interest = variable_of_interest,
                    include = include,
                    exclude = exclude,
                    returnAll = T)

  # Prune model informed by AIC and number of events in data
  if(length(models) > 1) model.pruned <- pruneModel(models,
                                                    keep = variable_of_interest,
                                                    dim_ratio = dim_ratio,
                                                    dim_ratio_lax = dim_ratio_lax,
                                                    verbose = F)
  else model.pruned <- models[[1]]

  model.opt <- model.pruned

  cat('\nFinal model:\n  ',
      paste(outcome,'~',paste(colnames(model.opt$model)[2:ncol(model.opt$model)], collapse = ' + ')),
      '\n\n')

  return(model.opt)
}
