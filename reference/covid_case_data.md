# Weekly reported COVID-19 cases for three US states

Weekly new COVID-19 cases for California, New York, and Ohio, covering
20 January 2020 to 27 March 2023, built by taking first differences of
the cumulative case counts reported by The New York Times and summing
them into Monday-starting ISO weeks (matching
[`wastewater_data`](https://mrc-ide.github.io/pika/reference/wastewater_data.md)).
Negative day-to-day differences (occasional downward revisions in the
source data) were floored at zero before summing. The New York Times
repository stopped updating in March 2023, which sets the end of this
series. See `data-raw/wastewater_data.R` for the full processing script.

## Usage

``` r
covid_case_data
```

## Format

A data frame with 485 rows and 3 variables:

- state:

  US state name

- date:

  date of the Monday starting the ISO week, in YYYY-MM-DD format

- cases:

  newly reported COVID-19 cases that week

## Source

<https://github.com/nytimes/covid-19-data>
