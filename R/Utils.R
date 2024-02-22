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
                       ' ' = '')

  vars <- unlist(strsplit(stringr::str_replace_all(formula,operation_chars),','))
  vars <- vars[!(vars=='')]
  vars <- vars[is.na(suppressWarnings(as.numeric(vars)))]

  return(vars)
}
