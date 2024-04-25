#' Extract Variables from a Formula
#'
#' @param formula Formula as a string.
#'
#' @return Variables extracted from the formula in a vector
#'
getFormulaVars <- function(formula) {
  operation_chars <- c('\\+' = ',',
                       '\\-' = ',',
                       '\\*' = ',',
                       '\\/' = ',',
                       '\\^' = ',',
                       '\\(' = ',',
                       '\\)' = ',',
                       '\\|' = ',',
                       ' ' = '',
                       '~' = '')

  vars <- unlist(strsplit(stringr::str_replace_all(formula, operation_chars),','))
  vars <- vars[!(vars=='')]
  vars <- vars[is.na(suppressWarnings(as.numeric(vars)))]

  return(vars)
}

#' Function for obtaining list of variables from a model
#'
#' @param model A model of any class containing a "model" attribute
#'
#' @return Returns a named list of the response variable (Response) and the independent variables (Terms).
#' @export
#'
getModelVars <- function(model) {
  response <- colnames(model$model)[1]
  predictors <- colnames(model$model)[2:ncol(model$model)]

  return(list(Response = response,
              Predictors = predictors))
}


#' Save a table as a CSV file
#'
#' @param table
#'
#' @return Saves a table to a CSV file in desired location and prints that location
#' @export
#'
saveTab <- function(table) {
  if(!inherits(table, c('matrix',
                        'data.frame',
                        'tibble'))) stop('Must be a matrix, data.frame, or tibble.')

  save_directory <- rstudioapi::selectDirectory()
  save_path <- rstudioapi::selectFile(path = save_directory)
  if(is.null(save_path)) save_path <- file.path(save_directory,
                                                paste0(readline('Figure name: '),'.csv'))

  write.csv(table, file = file.path(save_path), row.names = F)

  cat('Table was saved to:\n  ', save_path)
}
