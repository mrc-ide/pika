# Convert a count time series to fractional change relative to a baseline period

For each group, computes the mean of `count_var` over a baseline period
of `n_baseline_periods` consecutive time steps starting at `start_date`
(or the earliest date if `start_date` is `NULL`). Each observation is
then expressed as a fractional change relative to that baseline mean:

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

  A data frame containing a count column, a date column, and a grouping
  column.

- date_var:

  Character string giving the name of the date column (class `Date`).
  Default is `"date"`.

- grp_var:

  Character string giving the name of the grouping column. The baseline
  mean is computed separately per group.

- count_var:

  Character string giving the name of the count column.

- n_baseline_periods:

  Integer. Number of consecutive time steps used to compute the baseline
  mean. For daily data, `7` gives a one-week baseline. Default is 7.

- start_date:

  Start date of the baseline period. Accepts a `Date` object or a
  character string in `"YYYY-MM-DD"` format (e.g. `"2020-01-13"`). If
  `NULL` (default), the earliest date across the combined dataset is
  used as the baseline start.

## Value

The input data frame with one additional numeric column, `perc_change`,
giving each observation as a fractional change relative to the
group-specific baseline mean (0 = no change from baseline, -1 = zero
counts, positive values = above baseline).

## Details

\$\$\texttt{perc\\change} = \frac{\texttt{count} - \texttt{baseline
mean}}{\texttt{baseline mean}}\$\$

A value of 0 indicates no change from baseline; -0.5 indicates a 50%
decrease; 1.0 indicates a doubling. Originally developed for population
mobility data but applicable to any non-negative count series.

## Examples

``` r
if (FALSE) { # \dontrun{
dat_pct <- calc_percent_change(
  dat                = mobility_data,
  date_var           = "date",
  grp_var            = "region",
  count_var          = "trips",
  n_baseline_periods = 7,
  start_date         = "2020-01-13"
)
} # }
```
