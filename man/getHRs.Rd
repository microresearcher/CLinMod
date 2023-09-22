% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHRs.R
\name{getHRs}
\alias{getHRs}
\title{Get Hazard Ratios}
\usage{
getHRs(
  data,
  event.time,
  event.status,
  variable,
  time.unit = c("days", "weeks", "months", "years"),
  sig.test = c("loglik", "wald")
)
}
\arguments{
\item{data}{Data table in data.frame format}

\item{event.time}{Name of column, in string format, containing time-to-event values}

\item{event.status}{Name of column, in string format, containing event status values (as either 0/1 or T/F)}

\item{variable}{Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values}

\item{time.unit}{Specify how the time units are displayed on KM graph. Defaults to days}

\item{sig.test}{Specify type of statistical test to use}
}
\value{
Plots a Kaplan-Meier plot and returns dataframe of Hazard Ratios and associated confidence intervals and p-values
}
\description{
Get Hazard Ratios
}