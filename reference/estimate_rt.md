# Estimate reproduction over time by group

This function estimates reproduction number by group using EpiEstim's
estimate_R() and then binds the results together into a single data
frame

## Usage

``` r
estimate_rt(
  dat,
  grp_var,
  date_var,
  incidence_var,
  est_method = "parametric_si",
  si_mean = 6.48,
  si_std = 3.83
)
```

## Arguments

- dat:

  data frame with columns that correspond to two time series and
  grouping variable(s)

- grp_var:

  character string of column names in dat to be used as grouping
  variable(s)

- date_var:

  primary time series (should be a column in dat)

- incidence_var:

  secondary time series (should be a column in dat)

- est_method:

  estimation method to be used to estimate R in EpiEstim:
  c("non_parametric_si","parametric_si","uncertain_si","si_from_data","si_from_sample")

- si_mean:

  mean of serial interval distribution to be specified when est_method =
  "parametric_si"

- si_std:

  standard deviation of serial interval distribution to be specified
  when est_method = "parametric_si"

## Value

data frame of mean, median, and 95
