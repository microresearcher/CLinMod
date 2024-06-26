% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getORs.R
\name{getOR.manual}
\alias{getOR.manual}
\title{Get the Odds Ratio for a single binary variable directly from the data}
\usage{
getOR.manual(
  data,
  response,
  variable,
  alpha = 0.05,
  longer = longer,
  repeatVar = F
)
}
\arguments{
\item{data}{Data table in data.frame format}

\item{response}{Name of column, in string format, containing boolean response values (as either 0/1 or T/F)}

\item{variable}{Name of column, in string format, for the variable of interest. Must also be 0/1 or T/F values}

\item{alpha}{Significance level. Defaults to 0.05}

\item{longer}{Whether to format the table into a longer format or nested format. Defaults to a nested format ("FALSE").}

\item{repeatVar}{Whether to repeat the variable name on the left-most column next to each category for categorical predictors. Defaults to "FALSE".}
}
\value{
Dataframe of Odds Ratio and associated confidence interval and p-value
}
\description{
Get the Odds Ratio for a single binary variable directly from the data
}
