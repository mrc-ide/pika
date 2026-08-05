# pika 0.3.0

## Bug fixes

* Fixed `calc_percent_change()` to validate `start_date` format explicitly instead of relying on `%in%` coercion between a character string and a `Date` column, whose behavior changed between R 4.5 and R 4.6 and caused a CI test failure on R release
* Fixed `calc_percent_change()` to qualify an unbound `counts` variable with `.data$` inside `summarise()`
* Fixed the "Case study: reproduction number and mobility in China" vignette, which had `x_var`/`y_var` swapped in its `cross_corr()` call and the date-shift direction reversed, so it silently returned a 0-day lag instead of reproducing the 4-day Hubei lag reported in Ainslie et al. (2020)
* Excluded `.claude/` from the built package via `.Rbuildignore`

## New features

* Added `wastewater_data` and `covid_case_data`, weekly SARS-CoV-2 wastewater concentration and COVID-19 case counts for California, New York, and Ohio (2020-2023)
* Added a new vignette, "Case study: wastewater surveillance as a leading indicator for COVID-19 cases", demonstrating `cross_corr()`/`rolling_corr()` on real CDC NWSS and New York Times data, and testing whether wastewater's lead time over clinical cases held up after at-home testing became widespread in 2022
* Renamed `pika_vignette.Rmd` to `china-mobility-rt.Rmd` and reorganized both vignettes as parallel case studies under a "Case studies" pkgdown menu
* Added a new "Get started" vignette (`pika.Rmd`) with a fast tour of every exported function, replacing the previous "Get started" navbar link that pointed straight at the China case study

## Documentation

* Declared `snakecase` as a `Suggests` dependency (used in the China vignette but previously undeclared, which broke vignette building)
* Added literature citations (Peccia et al. 2020, Li et al. 2022, Boehm et al. 2023) putting the wastewater case study's findings in context with previously published results
* Added a Zenodo DOI badge and a license badge to the README

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
