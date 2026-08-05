# Estimate the effective reproduction number (Rt) over time by group

A grouped wrapper around
[`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) (Cori
et al. 2013). For each group, Rt is estimated in a sliding weekly window
using a Bayesian framework with a Gamma-distributed serial interval.
Results from all groups are combined into a single data frame.

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

  A data frame with at least a date column, an incidence column, and a
  grouping column. No `NA` values are permitted in `incidence_var`.

- grp_var:

  Character string giving the name of the grouping column. Rt is
  estimated independently for each group.

- date_var:

  Character string giving the name of the date column.

- incidence_var:

  Character string giving the name of the daily incidence (case count)
  column.

- est_method:

  Character string specifying the serial interval estimation method
  passed to
  [`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html). One
  of `"parametric_si"` (default), `"non_parametric_si"`,
  `"uncertain_si"`, `"si_from_data"`, or `"si_from_sample"`.

- si_mean:

  Mean of the serial interval distribution (days). Used when
  `est_method = "parametric_si"`. Default is 6.48 (COVID-19; Nishiura et
  al. 2020).

- si_std:

  Standard deviation of the serial interval distribution (days). Used
  when `est_method = "parametric_si"`. Default is 3.83 (COVID-19;
  Nishiura et al. 2020).

## Value

A data frame with one row per estimation window per group, containing:

- `date_start`:

  Start date of the estimation window.

- `date_end`:

  End date of the estimation window.

- `<grp_var>`:

  Group identifier; column name matches `grp_var`.

- `r_mean`:

  Posterior mean Rt.

- `r_median`:

  Posterior median Rt.

- `r_q2.5`:

  2.5th percentile of the posterior (lower 95% credible interval).

- `r_q97.5`:

  97.5th percentile of the posterior (upper 95% credible interval).

## Details

The default serial interval parameters (`si_mean = 6.48`,
`si_std = 3.83`) are from Nishiura et al. (2020) for COVID-19 and should
be updated for other pathogens.

## References

Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework and
software to estimate time-varying reproduction numbers during epidemics.
*American Journal of Epidemiology*, 178(9), 1505–1512.
[doi:10.1093/aje/kwt133](https://doi.org/10.1093/aje/kwt133)

Nishiura H, Linton NM, Akhmetzhanov AR (2020). Serial interval of novel
coronavirus (COVID-19) infections. *International Journal of Infectious
Diseases*, 93, 284–286.
[doi:10.1016/j.ijid.2020.02.060](https://doi.org/10.1016/j.ijid.2020.02.060)

## See also

[`estimate_R`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) for
full estimation control, including non-parametric serial intervals.

## Examples

``` r
if (FALSE) { # \dontrun{
rt_estimates <- estimate_rt(
  dat           = china_case_data,
  grp_var       = "province",
  date_var      = "date",
  incidence_var = "cases"
)
} # }
```
