# Calculate rolling (moving-window) correlation between two time series

Computes the Pearson correlation between `x_var` and `y_var` over a
rolling window of `n` time periods within each group, using
[`runCor`](https://rdrr.io/pkg/TTR/man/runFun.html). The first `n - 1`
observations in each group will be `NA` because there are insufficient
data to fill the window.

## Usage

``` r
rolling_corr(dat, date_var = "date", grp_var, x_var, y_var, n = 14)
```

## Arguments

- dat:

  A data frame containing the two time series, a date column, and a
  grouping column.

- date_var:

  Character string giving the name of the date column. Must be of class
  `Date`. Default is `"date"`. Rows are sorted by `date_var` within each
  group before the rolling correlation is computed (required because
  [`runCor`](https://rdrr.io/pkg/TTR/man/runFun.html) assumes row order
  is chronological).

- grp_var:

  Character string giving the name of the grouping column. Rolling
  correlation is computed separately within each group.

- x_var:

  Character string giving the name of the primary time series column.

- y_var:

  Character string giving the name of the secondary time series column.

- n:

  Integer. Width of the rolling window in time periods. Default is 14.

## Value

A data frame with the same columns as the input plus one additional
numeric column, `roll_corr`, containing the rolling Pearson correlation
between `x_var` and `y_var`. Values range from -1 to 1. The first
`n - 1` observations per group are `NA`. Note that rows where `x_var` or
`y_var` are `NA` are removed before the rolling correlation is computed,
so the returned frame may have fewer rows than the input.

## See also

[`cross_corr`](https://mrc-ide.github.io/pika/reference/cross_corr.md)
to identify the optimal lag before computing rolling correlation;
[`runCor`](https://rdrr.io/pkg/TTR/man/runFun.html) for the underlying
method;
[`plot_corr`](https://mrc-ide.github.io/pika/reference/plot_corr.md) to
visualise the result.

## Examples

``` r
if (FALSE) { # \dontrun{
data_corr <- rolling_corr(
  dat      = my_data,
  date_var = "date",
  grp_var  = "region",
  x_var    = "r_mean",
  y_var    = "movement",
  n        = 14
)
} # }
```
