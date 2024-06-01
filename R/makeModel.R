#' Make a Model
#'
#' @param data Data frame with patient IDs as rows and covariates as columns.
#' @param response Response variable. Should be the column name of the desired response variable in @data.
#' @param time For survival data, the time variable corresponding to the response.
#' @param predictors Vector of predictor/independent variables to select from when building the model. These should be column names in @data.
#' @param family Used in the glm function: "Type of error distribution and link function to be used in the model. For glm this can be a character string naming a family function, a family function or the result of a call to a family function. For glm.fit only the third option is supported. (See family for details of family functions.)". Defaults to binomial.
#'
#' @return A model.
#' @export
#'
makeModel <- function(data, response, time = NULL, predictors = c(),
                      family = 'binomial') {
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
    # Get all values in "response" that are not 0, 1, or missing
    na_response <- setdiff(data[[response]], c(0, 1, NA))

    # If event factor is not made of only 0, 1, and NA, then allow user to change it into such a factor
    if(length(na_response)) {
      message('"',response,'"',
              ' values must be 0 or 1 for "event" and "censored", respectively.\n The following additional values were found and these samples/cases:\n   ',
              paste(na_response, collapse = '\n   '))
      cat('\n')
      response.event <- select.list(setdiff(data[[response]], NA),
                                    title = 'Please select the value corresponding to events (e.g. death)')
      response.censored <- select.list(setdiff(data[[response]], NA),
                                       title = 'Please select the value corresponding to censored data (e.g. alive)')

      data[[response]] <- factor(data[[response]],
                                 levels = c(response.censored, response.event),
                                 labels = c(0, 1))
    }
  }

  # If at least 1 valid predictor was not specified, ask user to choose from dataframe columns
  #  Exception in the case of an intercept, where the predictor is 1
  predictors <- intersect(predictors, c(colnames(data), 1))
  while(length(predictors) < 1) predictors <- utils::select.list(setdiff(colnames(data), c(response, time)),
                                                                 multiple = T,
                                                                 title = 'Please select at least 2 potential predictor variables.')

  # Reduce data to only the rows with valid values for the time and/or response
  data.reduced <- data[complete.cases(data[c(time, response)]),]

  if(length(time)) {
    message('Creating a coxph model:')
    surv <- Surv(as.numeric(data.reduced[[time]]),
                 as.numeric(data.reduced[[response]]))

    model <- coxph(formula(paste('surv ~',
                                 paste(predictors, collapse = '+'))),
                   data = data.reduced, model = T)
    colnames(model$model)[1] <- response
    model$data <- data.reduced
  } else {
    message('Creating a glm model:')
    model <- glm(formula(paste(response,'~',
                               paste(predictors, collapse = '+'))),
                 data = data.reduced,
                 family = family)
  }

  class(model) <- append(class(model), 'clmodel')

  return(model)
}

#' User-friendly Wrapper Function to Add or Drop Predictors for a Model
#'
#' @param model Model of class "aov", "lm", "glm", "mlm", "coxph", and potentially other models. Passed to stats::update() through wrapper updateModel().
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
  updateModel(model, . ~ .)

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

  model <- updateModel(model,
                       formula(paste('. ~ .',
                                     ifelse(length(add),
                                            paste('+',paste(add, collapse = '+')),''),
                                     ifelse(length(drop),
                                            paste('-',paste(drop, collapse = '-')),''))))

  cat('\n=== NEW MODEL ===\n',
      paste(model$formula[2]),paste(model$formula[1]),paste(model$formula[3]),
      '\n')

  return(model)
}

#' Wrapper for update() Function
#'
#' @param model "An existing fit from a model function such as lm, glm and many others."
#' @param formula "Changes to the formula – see update.formula for details."
#' @param ... "Additional arguments to the call, or arguments with changed values. Use name = NULL to remove the argument name."
#' @param evaluate "If true evaluate the new call else return the call."
#'
#' @return Updated model as updated by the update() function
#' @export
#'
updateModel <- function(model, formula = . ~ ., ..., evaluate = T) {
  if(length(model$call$family)) {
    # Agnostic of whether a valid model object was passed
    model$call$family <- model$family$family
    return(update(model,
                  formula(deparse(formula)),
                  data = model$data,
                  ...,
                  evaluate = evaluate))
  } else if(inherits(model, 'coxph')) {
    if(!length(model$data)) stop('Please provide a coxph model generated by "".')
    formula <- stats::update.formula(model$formula,
                                     formula(deparse(formula)))

    model.new <- coxph(formula, data = model$data[3:ncol(model$data)], model = T)
    colnames(model.new$model)[1] <- colnames(model$model)[1]
    model.new$data <- model$data
    # model.new$model <- cbind(model$model[1],
    #                          model.new$data[colnames(model.new$data) %in% c(names(model.new$xlevels))])
    # model.new$call <- as.call(str2lang(paste0('coxph(formula = ',
    #                                           deparse(model.new$formula),
    #                                           ', model = model$data[3:ncol(model$data)])')))
    return(model.new)
  }
}
