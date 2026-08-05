# Package index

## Analysis Functions

Functions for analyzing relationships between time series

- [`cross_corr()`](https://mrc-ide.github.io/pika/reference/cross_corr.md)
  : Find the lag at which cross-correlation between two time series is
  maximised
- [`rolling_corr()`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)
  : Calculate rolling (moving-window) correlation between two time
  series
- [`estimate_rt()`](https://mrc-ide.github.io/pika/reference/estimate_rt.md)
  : Estimate the effective reproduction number (Rt) over time by group
- [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md)
  : Convert a count time series to fractional change relative to a
  baseline period

## Visualization

Functions for visualizing results

- [`plot_corr()`](https://mrc-ide.github.io/pika/reference/plot_corr.md)
  : Plot time series and rolling correlation over time by group
- [`plot_lag()`](https://mrc-ide.github.io/pika/reference/plot_lag.md) :
  Plot a histogram of optimal lags across groups

## Data

Example datasets included in the package

- [`china_case_data`](https://mrc-ide.github.io/pika/reference/china_case_data.md)
  : Daily confirmed cases of COVID-19 in China
- [`exante_movement_data`](https://mrc-ide.github.io/pika/reference/exante_movement_data.md)
  : Daily within-city movement data for different regions in China
