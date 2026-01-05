# pika 0.2.0

## Bug fixes

* Fixed bug in `estimate_rt()` where `date_end` was calculated incorrectly
* Fixed bug in `plot_lag()` where renamed data frame was not used
* Fixed type checking in `rolling_corr()` to use `inherits()` instead of `class()`
* Replaced deprecated `summarise_at()` with modern `summarise()` in `calc_percent_change()`

## New features

* Added pkgdown website at https://mrc-ide.github.io/pika/
* Added GitHub Actions workflows for R-CMD-check and pkgdown deployment

## Documentation

* Completely rewrote README with comprehensive usage examples
* Added citation to the associated Wellcome Open Research paper
* Fixed vignette index entry title
* Added URL and BugReports fields to DESCRIPTION

## Testing

* Added comprehensive test coverage for all functions
* Added happy path tests for `cross_corr()`, `rolling_corr()`, `estimate_rt()`, `calc_percent_change()`
* Added tests for `plot_lag()` (previously untested)
* Added tests for `plot_corr()` with confidence bands and customization options

# pika 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Added `calc_percent_change()` to calculate the percent change in a time series relative to a baseline period
