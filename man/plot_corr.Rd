% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_corr}
\alias{plot_corr}
\title{This function plots the two time series and correlation over time
There are optional arguments for confidence bands on x_var and customisation of the
maximum value of the y-axis and labels for the facets of grp_var}
\usage{
plot_corr(
  dat,
  date_var,
  grp_var,
  x_var,
  y_var,
  x_var_lower = NULL,
  x_var_upper = NULL,
  facet_labels = NULL,
  legend_labels = NULL,
  y_max = NULL,
  col_values = c(brewer.pal(8, "RdPu")[8], brewer.pal(8, "Greens")[5], brewer.pal(8,
    "Blues")[8])
)
}
\arguments{
\item{dat}{data frame with columns that correspond to two time series and grouping variable(s)}

\item{date_var}{character string of the column name that corresponds to the date variable
date_var must be specified if subset_date is not null.}

\item{grp_var}{character string of column names in dat to be used as grouping variable(s)}

\item{x_var}{primary time series (should be a column in dat)}

\item{y_var}{secondary time series (should be a column in dat)}

\item{x_var_lower}{column in dat corresponding to lower bound of confidence band for x_var}

\item{x_var_upper}{column in dat corresponding to upper bound of confidence band for x_var}

\item{facet_labels}{vector of labels for facets (grp_var)}

\item{legend_labels}{character vector of labels to use for plot legend}

\item{y_max}{maximum value of y-axis}

\item{col_values}{vector of color values}
}
\value{
tibble of lags by grp_var
}
\description{
This function plots the two time series and correlation over time
There are optional arguments for confidence bands on x_var and customisation of the
maximum value of the y-axis and labels for the facets of grp_var
}
\keyword{pika}
