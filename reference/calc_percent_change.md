# Convert a count variable into percent change relative to baseline

This function converts a count variable over time into a percent change
based on the average value in the specified baseline period. This
function was written for application to mobility data, where the percent
change in mobility over time relative to baseline is of interest.
However, this function can be applied to any count type time series.

## Usage

``` r
calc_percent_change(
  dat,
  date_var = "date",
  grp_var,
  count_var,
  n_baseline_periods = 7,
  start_date = NULL
)
```

## Arguments

- dat:

  data frame with columns that correspond to count variable and grouping
  variable

- date_var:

  character string of the name of the date column (should be of class
  "Date")

- grp_var:

  character string of column name in dat to be used as grouping variable

- count_var:

  character string of the name of the count column, such as number of
  trips

- n_baseline_periods:

  Number of periods to calculate baseline average over. For example, if
  the time series is days, n_baseline_periods = 7 for a baseline week.

- start_date:

  start date of baseline period (character string)

## Value

data frame of with an additional column of the percent change relative
to baseline
