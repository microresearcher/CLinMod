#' User-friendly Wrapper Function to Add or Drop Predictors for a Model
#'
#' @param model Model of class "aov", "lm", "glm", "mlm", "coxph", and potentially other models. Passed to stats::update().
#' @param drop Vector of predictor(s) to drop from the model.
#' @param add Vector of predictor(s) to add to the model.
#' @param verbose Whether to print statements.
#'
#' @return Model with specified variables removed and added.
#' @export
#'
changeModel <- function(model,
                        drop = c(),
                        add = c(),
                        verbose = T) {
  # Check if "model" is a valid model by running it through the "update" function
  update(model, . ~ .)

  # Predictor and response variables in current model
  vars <- colnames(model$model)
  # Predictor variables in current model
  predictors <- vars[2:length(vars)]

  # If any variables named in "add" and "drop" are not column names in the data, let the user know
  if(any(!(c(add, drop) %in% colnames(model$data)))) warning('The following variable names were not found in the data and were ignored:\n    ',
                                                             paste(c(add, drop)[!(c(add, drop) %in% colnames(model$data))],
                                                                   collapse = '\n    '),'\n')

  # If any variables named in "add" are already in the model, let the user know
  if(any(add %in% vars)) warning('The following variables are already in the model:\n    ',
                                 paste(add[any(add %in% vars)],
                                       collapse = '\n    '),'\n')

  # If the response variable is named in "drop", let the user know
  if(any(drop %in% vars[1])) warning('Cannot remove "',vars[1],'" since it is the response variable in the model.\n')

  # Remove any variable names in "add" that are already in the model or not found in the column names of the data
  add <- add[(add %in% colnames(model$data)) & !(add %in% vars)]

  # Remove any variable names in "drop" that are not predictors in the model
  drop <- drop[drop %in% predictors]

  if(!length(c(add, drop))) message('\nNo changes will be made to the model.')
  else if(verbose) cat('\nMaking the following changes to the model ("+" = adding, "-" = removing):',
                       paste(ifelse(length(add), paste('\n  +',paste(add, collapse = '\n  + ')), '')),
                       paste(ifelse(length(drop), paste('\n  -',paste(drop, collapse = '\n  - ')), '')),
                       '\n')

  model <- update(model, formula(paste('. ~ .',
                                       ifelse(length(add), paste('+',paste(add, collapse = '+')),''),
                                       ifelse(length(drop), paste('-',paste(drop, collapse = '-')),''))))

  cat('\n=== NEW MODEL ===\n',
      paste(model$formula[2]),paste(model$formula[1]),paste(model$formula[3]),
      '\n')

  return(model)
}
