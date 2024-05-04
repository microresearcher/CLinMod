
#' Load Data from Excel File
#'
#' @return Data.frame of data loaded from Excel file.
#' @export
#'
load_data <- function(path) {
  if(missing(path)) path <- file.choose()
  cat('Loading data from:\n  ', path, '\n')

  data.raw <- readxl::read_excel(path = path)

  print(head(data.raw))

  return(data.raw)
}

#' Clean data by re-classing columns
#'
#' @param data
#'
#' @return Data.frame with columns classed as either "factor", "numeric", or "Date"
#' @export
#'
clean_data <- function(data) {
  unclassed <- sapply(data, class)

  if(select.list(c('Yes', 'No'), title = 'Do you want to keep each column in the data?') == 'No') {
    if(select.list(c('Yes', 'No'),
                   title = 'In the next step, you will be asked for which columns to remove. Do you want to proceed?') == 'Yes') {
      ignore <- select.list(names(unclassed), multiple = T,
                            title = 'Select the columns that should removed from the data table:')
      unclassed <- unclassed[setdiff(names(unclassed), ignore)]
    }
  }

  data.classed <- data[names(unclassed)]

  # Ask user for columns that uniquely identify each sample/case and make these columns factors
  id_cols <- select.list(names(unclassed), multiple = T,
                         title = 'Select the column or columns that uniquely identify each sample/case:')
  data.classed[id_cols] <- lapply(id_cols, function(i) factor(data[[i]]))
  unclassed <- unclassed[setdiff(names(unclassed), id_cols)]

  # Identify columns with 'POSIX' class which should be dates according to user
  var.dates <- names(unclassed[grep('POSIX', unclassed)])
  data.classed[var.dates] <- lapply(data.classed[var.dates], as.Date)
  # Ask user for any other columns containing dates
  var.dates.forced <- c(select.list(setdiff(names(unclassed), var.dates), multiple = T,
                                    title = 'Select the column or columns that contain dates:'))
  # Any zeroes in user-selected columns should be turned into NAs
  data.classed[var.dates.forced][data.classed[var.dates.forced] == 0] <- NA
  # Now convert these selected columns into dates using Excel's default origin date
  data.classed[var.dates.forced] <- lapply(data.classed[var.dates.forced],
                                           function(d) suppressWarnings(openxlsx::convertToDate(d, origin = '1900-01-01')))

  # Identify columns that could not be turned into dates
  unsuccessful <- sapply(data.classed, class)[c(var.dates, var.dates.forced)] != 'Date'
  # Remove any columns that couldn't be turned into dates from the var.dates and var.dates.forced lists
  var.dates <- setdiff(var.dates, names(unsuccessful[unsuccessful]))
  var.dates.forced <- setdiff(var.dates.forced, names(unsuccessful[unsuccessful]))
  # Give a warning message if there were any columns that could not be turned into dates
  if(any(unsuccessful)) warning('The following columns could not be converted to dates:\n  ',
                                paste0(names(unsuccessful), collapse = '\n  '))

  unclassed <- unclassed[setdiff(names(unclassed), c(var.dates, var.dates.forced))]

  var.nums <- names(unclassed[unclassed == 'numeric'])

  # Any numeric column with, on average, less than 3 copies of each non-missing value will be considered continuous
  continuous <- var.nums[sapply(data.classed[var.nums],
                                function(c) length(unique(na.omit(c))) >= length(na.omit(c)) / 3)]
  unclassed <- unclassed[setdiff(names(unclassed), continuous)]

  # The remaining columns were labeled as character vectors
  #   Ask the user if any of them are continuous data and thus should be attempted to be coerced into numeric vectors
  # select.list(c('Proceed'), title = 'In the next step, you will be asked if any of the remaining columns contain continuous data.')
  continuous.forced <- c(select.list(names(unclassed), multiple = T,
                                     title = 'Do any of the following columns contain continuous data?'))

  # Remove the continuous.forced columns from unclassed
  unclassed <- unclassed[setdiff(names(unclassed), continuous.forced)]

  # Any remaining unclassed columns will become factors with the other categorical variables
  categorical <- names(unclassed)

  data.classed[c(continuous, continuous.forced)] <- lapply(data.classed[c(continuous, continuous.forced)], as.numeric)
  data.classed[categorical] <- lapply(data.classed[categorical], as.factor)

  return(data.classed)
}

#' Change the values of levels of factor variables in data
#'
#' @param data Data
#' @param variables Variables to refactor
#'
#' @return Dataframe with refactored variables
#' @export
#'
refactor <- function(data, variables) {
  variables <- intersect(variables, colnames(data))

  for(v in variables) {
    v.old <- data[[v]]
    if(!is.factor(v.old)) {
      warning(v.old, ' is not a factor')
      next
    }
    cat(paste0('For "',v,'"'))
    levels.new <- sapply(levels(v.old), function(l) readline(paste(' ', l, 'should be replaced by: ')))
    v.new <- factor(unname(sapply(v.old, function(x) levels.new[x])),
                    levels = unname(levels.new))

    data[[v]] <- v.new
  }

  return(data)
}
