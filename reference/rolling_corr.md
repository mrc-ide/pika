# Calculate rolling correlation between two time series by group

This function calculates the rolling correlation between two time series

## Usage

``` r
rolling_corr(dat, date_var = "date", grp_var, x_var, y_var, n = 14)
```

## Arguments

- dat:

  data frame with columns that correspond to two time series and
  grouping variable

- date_var:

  character string of date column name (should be of class "Date")

- grp_var:

  character string of column name in dat to be used as grouping variable

- x_var:

  primary time series (should be a column in dat)

- y_var:

  secondary time series (should be a column in dat)

- n:

  the number of time periods over which to calculate rolling correlation

## Value

tibble of lags by grp_var
