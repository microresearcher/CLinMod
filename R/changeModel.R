#' Add or Remove predictors for a model
#'
#' @param model Model of class "aov", "lm", "glm", or "mlm".
#' @param remove Vector of predictor(s) to remove from the model.
#' @param add Vector of predictor(s) to add to the model.
#'
#' @return Model with specified predictors removed and added.
#' @export
#'
changeModel <- function(model,
                        remove = c(),
                        add = c()) {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  vars <- colnames(model$model[0,])

  outcome <- vars[1]
  predictors <- vars[2:length(vars)]

  predictors <- predictors[!(predictors %in% remove)]
  add <- add[!(add %in% vars)]

  model.new <- model.lowestAIC <- glm(formula(paste(outcome,'~',
                                                    paste(c(predictors, add), collapse = ' + '))),
                                      data = model$data,
                                      family = model$family)

  return(model.new)
}
