# Weekly SARS-CoV-2 wastewater concentration for three US states

Weekly, state-level SARS-CoV-2 RNA concentration in wastewater for
California, New York, and Ohio, covering 20 April 2020 to 27 March 2023.
Built from raw, site-level sample records from the CDC National
Wastewater Surveillance System (NWSS) by taking a population-weighted
mean of the flow- and population-normalized concentration
(`pcr_target_flowpop_lin`, copies per person per day) across all sites
reporting in a state in a given week (Monday-starting ISO week). Sample
records with a missing concentration or population served, or a PCR
target other than SARS-CoV-2, were excluded before aggregation. The
number of contributing sites grew substantially over the course of the
program, from a handful of sites in 2020 to well over 100 per state by
2022-2023; see `n_sites` and `n_samples`. See
`data-raw/wastewater_data.R` for the full processing script.

## Usage

``` r
wastewater_data
```

## Format

A data frame with 430 rows and 5 variables:

- state:

  US state name

- date:

  date of the Monday starting the ISO week, in YYYY-MM-DD format

- conc:

  population-weighted mean SARS-CoV-2 concentration across sites
  reporting that week (copies per person per day, flow- and
  population-normalized)

- n_sites:

  number of distinct wastewater treatment sites contributing a sample
  that week

- n_samples:

  number of individual sample records contributing that week

## Source

<https://data.cdc.gov/resource/j9g8-acpt>
