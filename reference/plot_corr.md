# This function plots the two time series and correlation over time There are optional arguments for confidence bands on x_var and customisation of the maximum value of the y-axis and labels for the facets of grp_var

This function plots the two time series and correlation over time There
are optional arguments for confidence bands on x_var and customisation
of the maximum value of the y-axis and labels for the facets of grp_var

## Usage

``` r
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
```

## Arguments

- dat:

  data frame with columns that correspond to two time series and
  grouping variable(s)

- date_var:

  character string of the column name that corresponds to the date
  variable date_var must be specified if subset_date is not null.

- grp_var:

  character string of column names in dat to be used as grouping
  variable(s)

- x_var:

  primary time series (should be a column in dat)

- y_var:

  secondary time series (should be a column in dat)

- x_var_lower:

  column in dat corresponding to lower bound of confidence band for
  x_var

- x_var_upper:

  column in dat corresponding to upper bound of confidence band for
  x_var

- facet_labels:

  vector of labels for facets (grp_var)

- legend_labels:

  character vector of labels to use for plot legend

- y_max:

  maximum value of y-axis

- col_values:

  vector of color values

## Value

tibble of lags by grp_var
