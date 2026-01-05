# Changelog

## pika 0.2.0

### Bug fixes

- Fixed bug in
  [`estimate_rt()`](https://mrc-ide.github.io/pika/reference/estimate_rt.md)
  where `date_end` was calculated incorrectly
- Fixed bug in
  [`plot_lag()`](https://mrc-ide.github.io/pika/reference/plot_lag.md)
  where renamed data frame was not used
- Fixed type checking in
  [`rolling_corr()`](https://mrc-ide.github.io/pika/reference/rolling_corr.md)
  to use [`inherits()`](https://rdrr.io/r/base/class.html) instead of
  [`class()`](https://rdrr.io/r/base/class.html)
- Replaced deprecated `summarise_at()` with modern `summarise()` in
  [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md)

### New features

- Added pkgdown website at <https://mrc-ide.github.io/pika/>
- Added GitHub Actions workflows for R-CMD-check and pkgdown deployment

### Documentation

- Completely rewrote README with comprehensive usage examples
- Added citation to the associated Wellcome Open Research paper
- Fixed vignette index entry title
- Added URL and BugReports fields to DESCRIPTION

### Testing

- Added comprehensive test coverage for all functions
- Added happy path tests for
  [`cross_corr()`](https://mrc-ide.github.io/pika/reference/cross_corr.md),
  [`rolling_corr()`](https://mrc-ide.github.io/pika/reference/rolling_corr.md),
  [`estimate_rt()`](https://mrc-ide.github.io/pika/reference/estimate_rt.md),
  [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md)
- Added tests for
  [`plot_lag()`](https://mrc-ide.github.io/pika/reference/plot_lag.md)
  (previously untested)
- Added tests for
  [`plot_corr()`](https://mrc-ide.github.io/pika/reference/plot_corr.md)
  with confidence bands and customization options

## pika 0.1.1

- Added a `NEWS.md` file to track changes to the package.
- Added
  [`calc_percent_change()`](https://mrc-ide.github.io/pika/reference/calc_percent_change.md)
  to calculate the percent change in a time series relative to a
  baseline period
