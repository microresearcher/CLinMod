% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeOptModel.R
\name{optModel.AIC}
\alias{optModel.AIC}
\title{Optimize a Model using AIC}
\usage{
optModel.AIC(
  model,
  predictors,
  keep = c(),
  direction = c("both", "prune", "build"),
  limitDim = T,
  dim_ratio = 10,
  dim_ratio_lax = 0,
  verbose = T,
  trace = T
)
}
\arguments{
\item{model}{Model of class "aov", "lm", "glm", "mlm", "coxph", and potentially other models. Passed to stats::update() through wrapper updateModel().}

\item{predictors}{Vector of all predictor/independent variables to consider including in the model. These should be column names in @model$data.}

\item{keep}{(Optional) Vector of predictor variable(s) listed in @predictors that must be included in the model.}

\item{direction}{The mode of stepwise model creation. Can be one of "build", "prune", or "both". Defaults to "both".}

\item{limitDim}{Whether to limit the dimensionality (size) of the model based on the number of observations in the data.}

\item{dim_ratio}{Number used in the pruning step to determine the highest allowed dimensionality of the model (how many covariates the model is allowed to have on the right-hand side). The number of events in the data divided by @dim_ratio (rounded down to the nearest whole number) yields the highest dimensionality allowed. Defaults to 10.}

\item{dim_ratio_lax}{How many more dimensions is the model allowed to have than that calculated using @dim_ratio_lax. For example, for a dataset with 26 events and a @dim_ratio of 10, 26/10 = 2.6. A @dim_ratio_lax of 0 in this case would prune a model to 2 dimensions, while a @dim_ratio_lax of 1 would prune the model to 3 dimensions (assuming a model with at least 3 dimensions was able to be built).}

\item{verbose}{Whether to print out all messages such as final model. Defaults to TRUE. Details of each optimization step will always be printed out.}

\item{trace}{Whether to print out all AICs at each step. Defaults to TRUE.}
}
\value{
Returns an optimized model informed by stepwise AIC minimization.
}
\description{
Optimize a Model using AIC
}
