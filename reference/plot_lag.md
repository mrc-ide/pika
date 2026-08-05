# Plot a histogram of optimal lags across groups

Produces a histogram of the lag values returned by
[`cross_corr`](https://mrc-ide.github.io/pika/reference/cross_corr.md),
showing the distribution of optimal lags across groups.

## Usage

``` r
plot_lag(dat, lag_var, bins = 2)
```

## Arguments

- dat:

  A data frame containing a lag column, typically the output of
  [`cross_corr`](https://mrc-ide.github.io/pika/reference/cross_corr.md).

- lag_var:

  Character string giving the name of the lag column to plot.

- bins:

  Numeric. Bin width for the histogram. Default is 2.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`cross_corr`](https://mrc-ide.github.io/pika/reference/cross_corr.md)
to compute the lag values;
[`plot_corr`](https://mrc-ide.github.io/pika/reference/plot_corr.md) to
visualise the time series and rolling correlation.

## Examples

``` r
if (FALSE) { # \dontrun{
lags <- cross_corr(
  dat     = my_data,
  grp_var = "region",
  x_var   = "r_mean",
  y_var   = "movement"
)
plot_lag(lags, lag_var = "lag")
} # }
```
