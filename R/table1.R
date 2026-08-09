# data frame with desired variables, vectors of groupings
#' Generate a "Table1" Table
#'
#' @param data Can be either a dataframe or a model containing a "model" attribute.
#' @param characteristics List of characteristics to be summarized in the table. Should be column names in data.
#' @param group_by Column name in data that specifies a factor to group summarizing statistics.
#' @param props_within_group Whether to calculate proportions of categorical variables within each group or across all groups.
#'  Defaults to across all groups.
#' @param formula (Optional) Formula specifying which variables in data to show and any stratification if desired.
#' @param exclude (Optional) Variables in data that are to be excluded, if any.
#' @param signif Number of significant digits to report for statistical values. Defaults to 3.
#' @param stats Whether to test for significant difference between groups.
#' @param save_to_file Whether to offer to save to a file. Table will be shown first. Defaults to False.
#'
#' @return Data.frame and optional saved csv of a summary of sample characteristics for the provided data.
#' @export
#'
table1 <- function(data, characteristics = c(), group_by,
                   props_within_group = F,
                   formula = NULL, exclude = c(),
                   signif = 3,
                   stats = F,
                   save_to_file = F) {
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

  stratifier <- intersect(group_by, categorical)
  if(is.null(stratifier)) stop('Please provide a categorical variable in data by which to group statistical summaries')

  categorical <- intersect(categorical, characteristics)
  continuous <- intersect(continuous, characteristics)
  if(!length(c(categorical, continuous))) stop('None of the specified characteristics were found in the data.')

  # Categorical variables
  # for each categorical variable, create a contingency (count) table, proportions table, and merge them
  tabs.cat <- dplyr::bind_rows(lapply(categorical, function(cat) {
    # Create count table and keep the grouping variable in the new column names
    counts <- data[c(cat, stratifier)] %>%
      dplyr::mutate('count' = 1) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(stratifier),
                         names_prefix = paste0(stratifier,'|'),
                         values_from = 'count',
                         values_fn = sum, values_fill = 0) %>%
      dplyr::arrange(.data[[cat]])

    # Create a proportions table, either as proportion of each sub-cohort within within each grouping
    #   or proportions of each grouping within each sub-cohort
    if(props_within_group) props <- cbind(counts[1],
                                          as.data.frame(sapply(counts[2:ncol(counts)],
                                                               function(x) x / sum(x))))
    else props <- cbind(counts[1],
                        counts[2:ncol(counts)] / rowSums(counts[2:ncol(counts)]))

    # Combine the counts and proportions tables
    temp <- cbind(counts[1],
                  as.data.frame(matrix(paste0(as.matrix(counts[2:ncol(counts)]),
                                              ' (',
                                              100 * signif(as.matrix(props[2:ncol(props)]),
                                                           digits = signif),
                                              '%)'),
                                       nrow = nrow(counts))))

    # Rename the column names
    colnames(temp) <- c('Group', colnames(counts)[2:ncol(counts)])

    temp$Group <- as.character(temp$Group)

    # Add an additional row that includes the name of the characteristic (variable)
    #   This will serve as a sub-header in the final table
    tab <- cbind('Variable' = c(cat, rep('', nrow(temp))),
                 rbind(rep('', ncol(temp)),
                       temp))

    # If stats are requested, perform chi-squared test
    if(stats) {
      tab <- cbind(tab, descriptive_stats(data = data,
                                          characteristic = cat,
                                          group_by = stratifier,
                                          var_type = 'categorical',
                                          paired = F)$p.value)

      colnames(tab)[ncol(tab)] <- 'p-value'

      # Do not need the p-value written on each row, just the first row (the sub-header row with the variable name)
      tab$`p-value`[2:nrow(tab)] <- ''
    }

    return(tab)
  }))

  tabs.cat[is.na(tabs.cat)] <- '<Missing>'

  # Continuous variables
  tabs.cont <- dplyr::bind_rows(lapply(continuous, function(cat) {
    temp <- data[c(cat, stratifier)] %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(stratifier),
                         names_prefix = paste0(stratifier,'|'),
                         values_from = tidyselect::all_of(cat),
                         values_fn = function(x) {
                           paste0(signif(mean(x, na.rm = T), digits = signif),
                                  ' (', signif(sd(x, na.rm = T), digits = signif), ')')
                         }) %>%
      dplyr::mutate('Variable' = cat, 'Group' = 'Mean (St Dev)', .before = 1)

    # If stats are requested, perform appropriate statistical test
    if(stats) {
      temp <- cbind(temp, descriptive_stats(data = data,
                                            characteristic = cat,
                                            group_by = stratifier,
                                            var_type = 'continuous',
                                            paired = F)$p.value)

      colnames(temp)[ncol(temp)] <- 'p-value'
    }

    return(temp)
  }))

  tab1 <- rbind(tabs.cat, tabs.cont)

  if(save_to_file) {
    print(tab1)
    cat('\n')
    if(select.list(c('Yes','No'),
                   title = 'Save table to csv file?') == 'Yes') saveTab(tab1)
  }

  return(tab1)
}

#' Calculate statistics for descriptive characteristics between groups
#'
#' @param data Dataframe of data.
#' @param characteristic Column name in data that specifies the variable being tested between groups.
#' @param group_by Column name in data that specifies a factor to group data. Usually the independent variable of interest.
#' @param var_type Type of variable specified by "characteristic". Must be either "categorical" or "continuous".
#' @param paired Whether or not samples in each group are paired or unpaired. Defaults to False.
#'
#' @return Statistics summary using Chi-squared test for categorical variables
#'    or the appropriate statistical test for continuous variables
#' @export
#'
descriptive_stats <- function(data,
                              characteristic,
                              group_by,
                              var_type = c('categorical',
                                           'continuous'),
                              paired = F) {

  var_type <- match.arg(var_type, c('categorical',
                                    'continuous'))

  if(var_type == 'categorical') {
    counts <- data[c(characteristic, group_by)] %>%
      dplyr::mutate('count' = 1) %>%
      tidyr::pivot_wider(names_from = tidyselect::all_of(group_by),
                         names_prefix = paste0(group_by,'|'),
                         values_from = 'count',
                         values_fn = sum, values_fill = 0) %>%
      dplyr::arrange(.data[[characteristic]])

    return(chisq.test(counts[2:ncol(counts)]))

  } else if(var_type == 'continuous') {
    if(length(levels(data[[group_by]])) > 2) {
      # Paired test of more than 2 groups
      if(paired) return(anova(as.formula(paste0(characteristic,'~',group_by)), data = data))
      # Unpaired test of more than 2 groups
      else return(kruskal.test(as.formula(paste0(characteristic,'~',group_by)), data = data))
    } else {
      # Paired test of 2 groups
      if(paired) return(t.test(as.formula(paste0(characteristic,'~',group_by)), data = data))
      # Unpaired test of 2 groups
      else return(wilcox.test(as.formula(paste0(characteristic,'~',group_by)), data = data))
    }
  }
}
