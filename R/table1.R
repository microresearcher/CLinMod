# data frame with desired variables, vectors of groupings
#' Generate a "Table1" Table
#'
#' @param data Can be either a dataframe or a model containing a "model" attribute.
#' @param characteristics List of characteristics to be summarized in the table. Should be column names in data.
#' @param grouping Column name in data that specifies a factor to group summarizing statistics.
#' @param formula (Optional) Formula specifying which variables in data to show and any stratification if desired.
#' @param exclude (Optional) Variables in data that are to be excluded, if any.
#' @param signif Number of significant digits to report for statistical values. Defaults to 3.
#'
#' @return Data.frame and optional saved csv of a summary of sample characteristics for the provided data.
#' @export
#'
table1 <- function(data, characteristics = c(), grouping,
                   formula = NULL, exclude = c(),
                   signif = 3) {
  # Check if data is a matrix or dataframe
  if(any(class(data) %in% c('matrix','data.frame'))) data <- as.data.frame(data)
  else if(class(data$model) %in% c('matrix','data.frame')) data <- as.data.frame(data$model)
  else stop('Must provide a matrix or dataframe of data, or a model with a "model" attribute containing a matrix or dataframe of data.')

  var.types <- sapply(data, class)
  var.survs <- names(var.types[var.types == 'Surv'])
  var.chars <- names(var.types[var.types == 'character'])

  # If data contains survival data, make those variables into factors
  data[var.survs] <- as.factor(sapply(var.survs, function(v) {
    factor(ifelse(grepl('\\+', data[[v]]), 0, 1),
           levels = c(0, 1),
           labels = c('censored', 'event'))
  }))

  # Coerce any character vectors into numeric/integer if possible
  data[var.chars] <- lapply(var.chars, function(v) type.convert(data[[v]], as.is = T))

  # Update var.types and identify any remaining character vectors
  var.types <- sapply(data, class)
  var.chars <- names(var.types[var.types == 'character'])

  # Turn any remaining character vectors into factors
  data[var.chars] <- as.factor(sapply(var.chars, function(v) factor(data[[v]])))

  categorical <- names(data)[sapply(data, is.factor)]
  continuous <- names(data)[sapply(data, is.numeric)]
  # continuous <- setdiff(names(data), categorical)

  stratifier <- intersect(grouping, categorical)
  if(is.null(stratifier)) stop('Please provide a categorical variable in data by which to group statistical summaries')

  categorical <- intersect(categorical, characteristics)
  continuous <- intersect(continuous, characteristics)
  if(!length(c(categorical, continuous))) stop('None of the specified characteristics were found in the data.')

  # Categorical variables
  tabs.cat <- dplyr::bind_rows(lapply(categorical, function(cat) {
    counts <- data[c(cat, stratifier)] %>%
      dplyr::mutate('count' = 1) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(stratifier),
                         values_from = 'count',
                         values_fn = sum, values_fill = 0) %>%
      dplyr::arrange(.data[[cat]])

    props <- cbind(counts[1],
                   counts[2:ncol(counts)] / rowSums(counts[2:ncol(counts)]))

    temp <- cbind(counts[1],
                  as.data.frame(matrix(paste0(as.matrix(counts[2:ncol(counts)]),
                                              ' (',
                                              100 * signif(as.matrix(props[2:ncol(props)]),
                                                           digits = signif),
                                              '%)'),
                                       nrow = nrow(counts))))

    colnames(temp) <- c('Group', colnames(counts)[2:ncol(counts)])
    temp$Group <- as.character(temp$Group)

    tab <- cbind('Variable' = c(cat, rep('', nrow(temp))),
                 rbind(c('', '', ''),
                       temp))

    return(tab)
  }))

  tabs.cat[is.na(tabs.cat)] <- '<Missing>'

  # Continuous variables
  tabs.cont <- dplyr::bind_rows(lapply(continuous, function(cat) {
    data[c(cat, stratifier)] %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(stratifier),
                         values_from = all_of(cat),
                         values_fn = function(x) {
                           paste0(signif(mean(x, na.rm = T), digits = signif),
                                  ' (', signif(sd(x, na.rm = T), digits = signif), ')')
                         }) %>%
      dplyr::mutate('Variable' = cat, 'Group' = 'Mean (St Dev)', .before = 1)
  }))

  tab1 <- rbind(tabs.cat, tabs.cont)

  return(tab1)
}
