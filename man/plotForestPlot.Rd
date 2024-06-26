% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotForestPlot.R
\name{plotForestPlot}
\alias{plotForestPlot}
\title{Internal Forest Plot Function}
\usage{
plotForestPlot(
  data,
  n_breaks,
  var.rename = c(` ` = " "),
  title = NULL,
  var_label_position = "top",
  annotateStats = F,
  annotatePVal = F,
  annotateEffect = F,
  statistic = "Statistic"
)
}
\arguments{
\item{data}{Data for plot, with values and lower and upper bounds of confidence intervals for each level within each variable. Does not need to include baseline.}

\item{n_breaks}{Number of axis ticks to create on the x-axis of the plot.}

\item{var.rename}{Named vector to rename predictors with more appropriate names for a visual, if any need to be changed. For example, a column called "responded_to_treatment" could be renamed by inputting "c('responded_to_treatment'='Responded').}

\item{title}{Title of the generated figure.}

\item{var_label_position}{When the plot is split by predictors, this specifies whether the variable labels for each section of the plot are on the "top", "bottom", "right", or "left".}

\item{annotateStats}{Whether to add annotations for the stat results (e.g. OR, HR, etc) and confidence intervals. Defaults to False}

\item{annotatePVal}{Whether to add annotations for p-values for each group. Defaults to False}

\item{annotateEffect}{Whether to annotations "helpful" and "harmful" on each side of the y-axis. Defaults to False}
}
\value{
Forest plot returned to the calling function, either plotORs or plotHRs
}
\description{
Internal Forest Plot Function
}
