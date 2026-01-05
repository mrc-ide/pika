# This function plots the lags by grp_var

This function plots the lags by grp_var

## Usage

``` r
plot_lag(dat, lag_var, bins = 2)
```

## Arguments

- dat:

  data frame with columns that correspond to two time series and
  grouping variable(s)

- lag_var:

  character string of the column name that corresponds to the date
  variable

- bins:

  character string of column names in dat to be used as grouping
  variable(s)

## Value

tibble of lags by grp_var
