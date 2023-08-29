
#' Plot ORs
#'
#' @param model
#' @param n_breaks
#' @param var.rename
#' @param title
#'
#' @return
#' @export
#'
plotORs <- function(model, n_breaks=7, var.rename=c(), title=NULL, var_label_position='right') {
  valid_classes <- c('aov','lm','glm','mlm')

  if(!any(class(model) %in% valid_classes)) stop(cat('Model must be one of the following classes:\n ',
                                                     paste(valid_classes, collapse = '\n  ')))

  res <- getORs(model, repeatVar = T)

  res <- res[res$Variable != '(Intercept)',]
  res[3:6] <- sapply(res[3:6], as.numeric)
  res[is.na(res)] <- 1

  # Using 'unlist' incase user passed a list with 'list()' instead of named vector with 'c()'
  var.rename <- unlist(var.rename[names(var.rename) %in% res$Variable])
  if(length(var.rename)) res$Variable <- stringr::str_replace_all(res$Variable, var.rename)

  # This is to force ggplot2 to order the variable groups as they are in the table, with the help of tidytext package
  res$order <- rev(1:nrow(res))

  p <- ggplot(data = res, aes(y=tidytext::reorder_within(Level, order, Variable), x=OR, label=Level))+
    geom_point(size=4, shape=15)+
    geom_errorbarh(aes(xmin=`2.5%`, xmax=`97.5%`), height=.3)+
    geom_vline(xintercept=1, linetype='longdash')+
    facet_wrap(~Variable, ncol=1, scales = 'free_y', strip.position = var_label_position)+
    scale_x_continuous(trans = 'log',
                       breaks = scales::log_breaks(n = n_breaks),
                       labels = scales::label_number(accuracy = 0.1),
                       expand = c(0.1,0.1))+
    tidytext::scale_y_reordered()+
    theme_sa()+
    theme(axis.title.y = element_blank())+
    labs(x='Odds Ratio (Log-scaled)')

  if(var_label_position %in% c('top','bottom')) p <- p+theme(strip.text = element_text(hjust = 0))
  if(!is.null(title)) p <- p+labs(title=title)

  return(p)
}


#' Local ggplot2 theme
#'
#' @return
#' @export
#'
theme_sa <- function() {
  theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, size = 30),
          axis.title = element_text(size=25),
          axis.text = element_text(size=22),
          strip.text = element_text(size=25),
          strip.background = element_rect(fill = 'white', color = NA))
}
