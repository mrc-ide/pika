# Package index

## Analysis Functions

Functions for analyzing relationships between time series

- [`cross_corr()`](https://mrc-ide.github.io/pika/reference/cross_corr.md)
  : Determine cross correlation between two time series for different
  lags
- [`rolling_corr()`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)
  : Calculate rolling correlation between two time series by group
- [`estimate_rt()`](https://mrc-ide.github.io/pika/reference/estimate_rt.md)
  : Estimate reproduction over time by group
- [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md)
  : Convert a count variable into percent change relative to baseline

## Visualization

Functions for visualizing results

- [`plot_corr()`](https://mrc-ide.github.io/pika/reference/plot_corr.md)
  : This function plots the two time series and correlation over time
  There are optional arguments for confidence bands on x_var and
  customisation of the maximum value of the y-axis and labels for the
  facets of grp_var
- [`plot_lag()`](https://mrc-ide.github.io/pika/reference/plot_lag.md) :
  This function plots the lags by grp_var

## Data

Example datasets included in the package

- [`china_case_data`](https://mrc-ide.github.io/pika/reference/china_case_data.md)
  : Daily confirmed cases of COVID-19 in China
- [`exante_movement_data`](https://mrc-ide.github.io/pika/reference/exante_movement_data.md)
  : Daily within-city movement data for different regions in China
