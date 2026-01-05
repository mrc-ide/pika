# Determine cross correlation between two time series for different lags

This function determines lag at which the cross correlation is highest

## Usage

``` r
cross_corr(
  dat,
  date_var = NULL,
  grp_var,
  x_var,
  y_var,
  max_lag = 20,
  subset_date = NULL
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

- max_lag:

  integer value of the number of lags to perform cross correlation for

- subset_date:

  a character string of the same format as the date variable

## Value

tibble of lags by grp_var
