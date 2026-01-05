# Daily within-city movement data for different regions in China

The daily within-city movement data, used as a proxy for economic
activity, is available from 1 January to 24 March 2020 for major
metropolitan cities within each province in mainland China, Hong Kong
SAR, and Macau SAR. These data, provided by Exante Data Inc, measured
travel activity relative to the 2019 average (excluding Lunar New Year).
The underlying data are based on near real-time people movement
statistics from Baidu.

## Usage

``` r
exante_movement_data
```

## Format

A data frame with 672 rows and 3 variables:

- date:

  date, in YYYY-MM-DD format

- province:

  name of province/region in China where cases occured

- movement:

  daily population-weighted with-in city movement index
