
#' Load Data from Excel or CSV File
#'
#' @param path Path to f
#' @param verbose
#'
#' @return Data.frame of data loaded from Excel or CSV file.
#' @export
#'
loadData <- function(path, verbose = T) {
  if(missing(path)) path <- file.choose()
  else if(!file.exists(path)) {
    # path <- dirname(path)
    # while(!dir.exists(path)) {
    #   path <- dirname(path)
    #   if(path == '/') break
    # }
    path <- file.choose()
  }
  if(verbose) cat('Loading data from:\n  ', path, '\n')

  ext <- tail(strsplit(basename(path), '.', fixed = T)[[1]], n = 1)

  if(grepl('xls', ext)) data.raw <- readxl::read_excel(path = path)
  else if(ext == 'csv') data.raw <- read.csv(file = path)

  if(all(grepl('#', data.raw[nrow(data.raw),]))) {
    if(verbose) message('Data classes for each column were found in the file and will be applied to each column.')
    classes <- data.raw[nrow(data.raw),]

    data.raw <- data.raw[1:(nrow(data.raw) - 1),]

    for(c in colnames(data.raw)) {
      if(classes[[c]] == '#integer') data.raw[[c]] <- as.integer(data.raw[[c]])
      if(classes[[c]] == '#factor') data.raw[[c]] <- factor(data.raw[[c]],
                                                            exclude = c(NA, '', 'NA', 'N/A'))
      if(classes[[c]] == '#Date') data.raw[[c]] <- as.Date(data.raw[[c]])
      if(classes[[c]] == '#numeric') data.raw[[c]] <- as.numeric(data.raw[[c]])
    }
  }

  return(data.raw)
}

#' Clean data by re-classing columns
#'
#' @param data
#'
#' @return Data.frame with columns classed as either "factor", "numeric", or "Date"
#' @export
#'
cleanData <- function(data) {
  save_name <- 'cleandata'

  column_names <- colnames(data)
  data <- as.data.frame(lapply(data, function(c) type.convert(c, as.is = T)))
  colnames(data) <- column_names

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

  ## Sample IDs ##
  # Ask user for columns that uniquely identify each sample/case and make these columns factors
  id_cols <- select.list(names(unclassed), multiple = T,
                         title = 'Select the column or columns that uniquely identify each sample/case:')
  data.classed[id_cols] <- lapply(id_cols, function(i) as.integer(data[[i]]))
  unclassed <- unclassed[setdiff(names(unclassed), id_cols)]

  ## Dates ##
  # Identify columns with 'POSIX' class which should be dates according to user
  var.dates <- names(unclassed[grep('POSIX|Date', unclassed)])
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

  ## Continuous ##
  var.nums <- names(unclassed[unclassed %in% c('numeric', 'integer')])
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

  ## Categorical ##
  # Any remaining unclassed columns will become factors with the other categorical variables
  categorical <- names(unclassed)
  data.classed[c(continuous, continuous.forced)] <- lapply(data.classed[c(continuous, continuous.forced)],
                                                           as.numeric)
  data.classed[categorical] <- lapply(data.classed[categorical],
                                      function(c) factor(c, exclude = c(NA, '', 'NA', 'N/A')))

  colnames(data.classed) <- renameVars(colnames(data.classed), auto = T)

  saveData(data.classed, filename = save_name)

  # message('\nPlease select a folder to save a csv file of the cleaned up data.')
  # save_path <- rstudioapi::selectDirectory()
  # if(length(save_path)) {
  #   write.csv(sapply(data.classed, as.character),
  #             file = file.path(save_path, paste0(save_name,'.csv')),
  #             row.names = F)
  #   cat('File saved to:', file.path(save_path, paste0(save_name,'.csv')))
  # } else warning('Cleaned data was not saved. You will neeed to go through this process again next time unless you save this R environment.')

  return(data.classed)
}

#' Clean up the factor variables in the data
#'
#' @param data Data
#' @param variables (Optional) Names of variables to rename.
#'    If not specified, will select all categorical variables in the data.
#' @param exclude (Optional) Names of variables to exclude.
#'    If this and "variables" are not specified, will select all categorical variables in the data.
#' @param all Whether or not to relabel all variables without asking before each one. Defaults to False.
#'
#' @return Dataframe with cleaned up factor variables
#' @export
#'
cleanFactors <- function(data, variables = NULL, exclude = NULL, all = F) {
  save_name <- 'cleaned_variable_data'
  # Make list of all categorical variables (factors in data that are not entirely made of unique values)
  vars.cat.all <- colnames(data)[sapply(data, function(v) {
    all(is.factor(v) & sum(duplicated(v)) > 0)
  })]
  if(!length(vars.cat.all)) stop('No factors found in the data.')

  vars <- setdiff(intersect(variables,
                            vars.cat.all),
                  exclude)

  if(all | !length(vars)) vars <- vars.cat.all

  for(v in vars) {
    cat(paste0('\nFor "',v,'"'))
    cat('\n  <> Unique values:', paste(unique(data[[v]]), collapse = ', '))
    cat('\n  <> Levels:', paste(levels(data[[v]]), collapse = ', '))

    labels.new <- relabel(data[[v]])
    data[[v]] <- labels.new$factor
    data[[paste0(v,'_comments')]] <- labels.new$comments
  }

  saveData(data, filename = save_name)

  # message('\nPlease select a folder to save a csv file of the data with cleaned up categorical variables.')
  # save_path <- rstudioapi::selectDirectory()
  # if(length(save_path)) {
  #   write.csv(sapply(data, as.character),
  #             file = file.path(save_path, paste0(save_name,'.csv')),
  #             row.names = F)
  #   cat('File saved to:', file.path(save_path, paste0(save_name,'.csv')))
  # } else warning('Cleaned data was not saved. You will neeed to go through this process again next time unless you save this R environment.')

  return(data)
}

#' Add or change labels of levels of a factor
#'
#' @param factor A factor to change the labels of
#'
#' @return Factor with relabeled levels
#' @export
#'
relabel <- function(factor) {
  if(!is.factor(factor)) {
    warning(factor, ' is not a factor')
    next
  }
  # Unfactor the factor. New factor will be made a factor at the end.
  factor.new <- as.vector(factor)

  values.new <- sapply(unique(factor.new)[order(unique(factor.new))],
                       function(u) readline(paste('', u, 'should be replaced by: ')))
  # Replace any "NA" or "N/A" responses with actual NA
  values.new[toupper(values.new) %in% c('NA', 'N/A')] <- NA
  # Identify all the values with duplicate assigned labels
  dupes <- values.new[values.new %in% unique(values.new[duplicated(values.new)])]
  if(length(dupes)) {
    warning(' Duplicate labels were given for different values. These values will be overwritten to reflect new labels.')
    # Create a new list of the original values that are being assigned to the same label
    factor_comments <- sapply(factor, function(v) ifelse(v %in% names(dupes),
                                                         dupes[names(dupes) == v],
                                                         ''))
    names(factor_comments) <- as.vector(factor)

    # Overwrite values
    factor.new[factor.new %in% names(dupes)] <- sapply(factor.new[factor.new %in% names(dupes)],
                                                       function(v) dupes[names(dupes) %in% v])

    # Rewrite values.new to remove the duplicated values now that they have been overwritten
    values.new <- values.new[!duplicated(values.new) & !is.na(values.new)]
  } else factor_comments <- NULL

  factor.new <- factor(factor.new,
                       labels = setdiff(unique(values.new), NA),
                       exclude = c(NA, '', 'NA', 'N/A'))

  return(list(factor = factor.new, comments = factor_comments))
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
    # if(select.list(c('Yes','No'),
    #                title = paste0('Add factor levels?')) == 'YES') {
    #   # levels.current <- levels(data[[v]])
    #   # levels.keep <- setdiff(levels.current, select.list(levels.current,
    #   #                                                    title = 'Select which levels to remove'))
    #   # data[[v]] <- factor(data[[v]], levels = levels.new)
    # }

    # if(select.list(c('Yes','No'),
    #                title = paste0('Delete any factor levels?')) == 'YES') {
    #   # levels.current <- levels(data[[v]])
    #   # levels.keep <- setdiff(levels.current, select.list(levels.current,
    #   #                                                    title = 'Select which levels to remove'))
    #   # data[[v]] <- factor(data[[v]], levels = levels.new)
    # }
    levels.new <- sapply(levels(v.old), function(l) readline(paste(' ', l, 'should be replaced by: ')))
    v.new <- factor(unname(sapply(v.old, function(x) levels.new[x])),
                    levels = unname(levels.new))

    data[[v]] <- v.new
  }

  return(data)
}

#' Save data to a csv file
#'
#' @param data Data in table, dataframe, or tibble format
#' @param folder Path of folder to save data file to
#' @param filename Name to give the data file
#'
#' @return Nothing
#' @export
#'
saveData <- function(data, folder, filename) {
  if(missing(folder)) folder <- ''
  if(!dir.exists(folder)) {
    message('\nPlease select a folder to save a csv file of the data.')
    folder <- rstudioapi::selectDirectory()
  }

  if(missing(filename)) filename <- NULL
  if(!length(filename)) filename <- readline('Filename: ')

  save_path <- file.path(folder, paste0(filename,'.csv'))

  classes <- paste0('#',unname(sapply(data, class)))

  if(length(folder)) {
    write.csv(rbind(sapply(data,
                           as.character),
                    classes),
              file = save_path,
              row.names = F)

    # Check that the saved data is the same
    data.saved <- loadData(save_path, verbose = F)
    colnames(data.saved) <- colnames(data)

    for(c in colnames(data)) {
      if(!identical(as.character(data[[c]]), as.character(data.saved[[c]]))) {
        stop('File was saved but some of the data was altered during the saving process.')
      }
    }

    cat('File saved to:', file.path(folder, paste0(filename,'.csv')))
  } else warning('Data was not saved to a file.
                 You will neeed to go through this process again next time unless you save this R environment or save the data manually.')
}

#' Rename Variables
#'
#' @param variables Vector of variables to rename.
#' @param auto Whether to automatically rename variables by replacing invalid characters only. Defaults to False.
#'
#' @return Named list of new variable names with names being the old variable names
#' @export
#'
renameVars <- function(variables, auto = F) {
  invalid_chars <- c(' '='_',
                     ':'='_',
                     '/'='_', '\''='',
                     '\\['='', '\\]'='',
                     '\\('='_', '\\)'='',
                     '"'='', ','='',
                     '\\$'='', '@'='', '#'='', '%'='', '\\^'='', '\\*'='',
                     '&'='_and_', '\\?'='',
                     '='='_equals_', '\\+'='_plus_', '\\-'='_minus_')
  if(!auto) {
    varnames.new <- sapply(variables,
                           function(v) readline(paste0('Please enter a new name for ', v, ': ')))
  } else varnames.new <- variables

  varnames.new <- stringr::str_replace_all(varnames.new, invalid_chars)

  dupes <- duplicated(varnames.new)

  if(any(dupes)) {
    warning(paste('Duplicates were found for the following names:\n  ',
                  paste(unique(varnames.new[dupes]), collapse = '\n  ')))
    varnames.new <- make.unique(varnames.new)

    message('The duplicates have been renamed as follows:\n  ',
            paste(varnames.new[dupes], collapse = '\n  '))
  }

  return(varnames.new)
}
