# pika ![](inst/pika_hex.png)

## Overview

**pika** is a lightweight R package for evaluating correlation between
time series. It was developed at the MRC Centre for Global Infectious
Disease Analysis for epidemiological applications, particularly for
studying relationships between reproduction numbers (Rt) and population
mobility during infectious disease outbreaks.

This package was developed as part of the research described in:

> Ainslie KEC, Walters CE, Fu H, et al. Evidence of initial success for
> China exiting COVID-19 social distancing policy after achieving
> containment. *Wellcome Open Research*. 2020;5:81. doi:
> [10.12688/wellcomeopenres.15843.2](https://doi.org/10.12688/wellcomeopenres.15843.2)

### Key Features

- **Cross-correlation analysis**: Determine the optimal lag between two
  time series where correlation is highest
- **Rolling correlation**: Calculate time-varying correlation between
  paired time series
- **Reproduction number estimation**: Wrapper around EpiEstim for
  group-wise Rt estimation
- **Percent change calculation**: Convert count variables to percent
  change relative to a baseline period
- **Visualization tools**: Plot time series with correlation overlays
  and lag distributions

## Installation

You can install the development version of pika from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("mrc-ide/pika")
```

### Integration with orderly

To use `pika` within an `orderly` task, install from the drat
repository:

``` r
drat:::add("ncov-ic")
install.packages("pika")
```

## Quick Start

``` r
library(pika)

# Load example data
data(china_case_data)
data(exante_movement_data)

# Estimate reproduction number by province
rt_estimates <- estimate_rt(
  dat = china_case_data,
  grp_var = "province",
  date_var = "date",
  incidence_var = "cases"
)

# Join Rt estimates with mobility data
data_joined <- dplyr::left_join(
  rt_estimates,
  exante_movement_data,
  by = c("date_end" = "date", "province")
)

# Determine optimal lag between Rt and movement
lags <- cross_corr(
  dat = data_joined,
  date_var = "date_end",
  grp_var = "province",
  x_var = "r_mean",
  y_var = "movement",
  max_lag = 10
)

# Calculate rolling correlation
data_corr <- rolling_corr(
  dat = data_joined,
  date_var = "date_end",
  grp_var = "province",
  x_var = "r_mean",
  y_var = "movement",
  n = 14
)

# Visualize results
plot_corr(
  dat = data_corr,
  date_var = "date_end",
  grp_var = "province",
  x_var = "r_mean",
  y_var = "movement"
)
```

## Functions

| Function                                                                                   | Description                                                                     |
|--------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| [`cross_corr()`](https://mrc-ide.github.io/pika/reference/cross_corr.md)                   | Determine the lag at which cross-correlation is highest between two time series |
| [`rolling_corr()`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)               | Calculate rolling (moving) correlation between two time series                  |
| [`estimate_rt()`](https://mrc-ide.github.io/pika/reference/estimate_rt.md)                 | Estimate effective reproduction number (Rt) by group using EpiEstim             |
| [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md) | Convert counts to percent change relative to a baseline period                  |
| [`plot_corr()`](https://mrc-ide.github.io/pika/reference/plot_corr.md)                     | Visualize time series and rolling correlation                                   |
| [`plot_lag()`](https://mrc-ide.github.io/pika/reference/plot_lag.md)                       | Plot histogram of lags by group                                                 |

## Included Data

pika includes example datasets from the COVID-19 pandemic in China:

- **china_case_data**: Daily confirmed COVID-19 cases by province (Jan
  16 - Mar 24, 2020)
- **exante_movement_data**: Daily within-city movement index by province
  (Jan 1 - Mar 24, 2020)

## Documentation

For detailed usage examples and methodology, see the package vignette:

``` r
vignette("pika_vignette", package = "pika")
```

## Citation

If you use pika in your research, please cite:

> Ainslie KEC, Walters CE, Fu H, et al. Evidence of initial success for
> China exiting COVID-19 social distancing policy after achieving
> containment. *Wellcome Open Research*. 2020;5:81. doi:
> [10.12688/wellcomeopenres.15843.2](https://doi.org/10.12688/wellcomeopenres.15843.2)

## License

MIT License. See [LICENSE](https://mrc-ide.github.io/pika/LICENSE.md)
for details.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull
requests on [GitHub](https://github.com/mrc-ide/pika).
