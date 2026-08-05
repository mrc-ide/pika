# Find the lag at which cross-correlation between two time series is maximised

Computes the cross-correlation function (CCF) between `x_var` and
`y_var` for lags from `-max_lag` to 0 using
[`ccf`](https://rdrr.io/r/stats/acf.html), then returns the lag with the
highest CCF for each group. Only non-positive lags are considered (i.e.
`x_var` leading `y_var`), reflecting the assumption that changes in the
primary series precede changes in the secondary series.

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

  A data frame containing the two time series and a grouping column.

- date_var:

  Character string giving the name of the date column. Required when
  `subset_date` is non-`NULL`. When supplied, rows are sorted by
  `date_var` within each group before the CCF is computed (required
  because [`ccf`](https://rdrr.io/r/stats/acf.html) assumes row order is
  chronological). If `NULL`, `dat` must already be sorted
  chronologically within each group.

- grp_var:

  Character string giving the name of the grouping column. The CCF is
  computed separately within each group.

- x_var:

  Character string giving the name of the primary (leading) time series
  column.

- y_var:

  Character string giving the name of the secondary (lagged) time series
  column.

- max_lag:

  Integer. Maximum number of lags to evaluate. CCF is computed for lags
  `-max_lag` to 0. Default is 20.

- subset_date:

  Character string in the same format as `date_var`. If supplied, only
  rows with dates on or before `subset_date` are used. Requires
  `date_var` to be specified.

## Value

A tibble with one row per group containing:

- `<grp_var>`:

  Group identifier; column name matches `grp_var`.

- `lag`:

  Integer \\\leq 0\\. The lag at which the CCF between `x_var` and
  `y_var` is highest within that group.

## See also

[`rolling_corr`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)
to compute rolling correlation at the identified lag;
[`ccf`](https://rdrr.io/r/stats/acf.html) for the underlying CCF method.

## Examples

``` r
if (FALSE) { # \dontrun{
lags <- cross_corr(
  dat      = my_data,
  date_var = "date",
  grp_var  = "region",
  x_var    = "r_mean",
  y_var    = "movement",
  max_lag  = 14
)
} # }
```
