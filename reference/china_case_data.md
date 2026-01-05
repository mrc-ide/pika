# Daily confirmed cases of COVID-19 in China

The case data has daily confirmed confirmed cases for different
provinces in China from 16 January to 24 March 2020 from the dashboard
maintained by Chinese Center for Disease Prevention and Control (CCDC).
The CCDC dashboard collates numbers of confirmed cases reported by
national and local health commissions in each province in mainland
China, and Hong Kong SAR and Macau SAR. Confirmed cases are defined as
suspected cases, who have epidemiological links and/or clinical
symptoms, and are detected with SARS-CoV-2 by PCR tests. However, in
Hubei province, clinically diagnosed cases were additionally included
between 12 and 19 February.

## Usage

``` r
china_case_data
```

## Format

A data frame with 483 rows and 3 variables:

- date:

  date, in YYYY-MM-DD format

- province:

  name of province/region in China where cases occured

- cases:

  number of daily cases reported of COVID-19

## Source

<http://2019ncov.chinacdc.cn/2019-nCoV/>
