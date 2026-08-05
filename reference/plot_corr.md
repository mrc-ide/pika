# Plot time series and rolling correlation over time by group

Produces a faceted line plot showing the primary series (`x_var`),
secondary series (`y_var`), and rolling correlation (`roll_corr`) over
time, with one facet per group. Horizontal reference lines are drawn at
y = -1 (dashed), 0 (solid), and 1 (dashed). Optionally adds a shaded
confidence ribbon around `x_var`. If `dat` contains a column named
`roll_corr` (e.g. from
[`rolling_corr`](https://mrc-ide.github.io/pika/reference/rolling_corr.md))
it will be included in the plot; if absent the line is simply omitted.

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

  A data frame containing the two time series, a `roll_corr` column, a
  date column, and a grouping column.

- date_var:

  Character string giving the name of the date column (class `Date`).

- grp_var:

  Character string giving the name of the grouping column used for
  faceting.

- x_var:

  Character string giving the name of the primary time series column.

- y_var:

  Character string giving the name of the secondary time series column.

- x_var_lower:

  Character string giving the name of the column containing the lower
  confidence bound for `x_var`. If `NULL` (default), no ribbon is drawn.
  Both `x_var_lower` and `x_var_upper` must be supplied to draw a
  ribbon.

- x_var_upper:

  Character string giving the name of the column containing the upper
  confidence bound for `x_var`. If `NULL` (default), no ribbon is drawn.

- facet_labels:

  Named character vector of display labels for the facets, passed to
  [`as_labeller`](https://ggplot2.tidyverse.org/reference/as_labeller.html).
  Names must match values in the grouping column. If `NULL` (default),
  raw group values are shown.

- legend_labels:

  Character vector of length 3 giving legend labels for `roll_corr`,
  `x_var`, and `y_var` respectively. If `NULL` (default), the internal
  metric names (`roll_corr`, `x_var`, `y_var`) are shown in the legend.

- y_max:

  Numeric. Maximum value for the y-axis. If supplied, the axis is set to
  `[-1, y_max]` and confidence bounds are clamped to this value. Default
  is `NULL` (auto-scaled).

- col_values:

  Character vector of length 3 specifying line colours for `roll_corr`,
  `x_var`, and `y_var` respectively. Defaults to dark purple, mid-green,
  and dark blue from RColorBrewer palettes.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`rolling_corr`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)
to compute `roll_corr`;
[`plot_lag`](https://mrc-ide.github.io/pika/reference/plot_lag.md) to
visualise the lag distribution.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_corr(
  dat           = data_corr,
  date_var      = "date_end",
  grp_var       = "province",
  x_var         = "r_mean",
  y_var         = "movement",
  x_var_lower   = "r_q2.5",
  x_var_upper   = "r_q97.5",
  legend_labels = c("Rolling correlation", "Rt", "Mobility")
)
} # }
```
