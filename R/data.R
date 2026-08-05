#' Daily confirmed cases of COVID-19 in China
#'
#' The case data has daily confirmed cases for different provinces in China from 16 January to 24 March 2020
#' from the dashboard maintained by Chinese Center for Disease Prevention and Control (CCDC). The CCDC dashboard
#' collates numbers of confirmed cases reported by national and local health commissions in each province in mainland
#' China, and Hong Kong SAR and Macau SAR. Confirmed cases are defined as suspected cases, who have epidemiological
#' links and/or clinical symptoms, and are detected with SARS-CoV-2 by PCR tests. However, in Hubei province,
#' clinically diagnosed cases were additionally included between 12 and 19 February.
#'
#' @format A data frame with 483 rows and 3 variables:
#'  \describe{
#'    \item{date}{date, in YYYY-MM-DD format}
#'    \item{province}{name of province/region in China where cases were reported}
#'    \item{cases}{number of daily cases reported of COVID-19}
#' }
#' @source \url{http://2019ncov.chinacdc.cn/2019-nCoV/}
"china_case_data"

#' Daily within-city movement data for different regions in China
#'
#' The daily within-city movement data, used as a proxy for economic activity, is available from 1 January to
#' 24 March 2020 for major metropolitan cities within each province in mainland China, Hong Kong SAR, and Macau SAR.
#' These data, provided by Exante Data Inc, measured travel activity relative to the 2019 average (excluding Lunar
#' New Year). The underlying data are based on near real-time people movement statistics from Baidu.
#'
#' @format A data frame with 672 rows and 3 variables:
#'  \describe{
#'    \item{date}{date, in YYYY-MM-DD format}
#'    \item{province}{name of province/region in China}
#'    \item{movement}{daily population-weighted within-city movement index}
#' }
"exante_movement_data"

#' Weekly SARS-CoV-2 wastewater concentration for three US states
#'
#' Weekly, state-level SARS-CoV-2 RNA concentration in wastewater for California,
#' New York, and Ohio, covering 20 April 2020 to 27 March 2023. Built from raw,
#' site-level sample records from the CDC National Wastewater Surveillance System
#' (NWSS) by taking a population-weighted mean of the flow- and population-normalized
#' concentration (\code{pcr_target_flowpop_lin}, copies per person per day) across all
#' sites reporting in a state in a given week (Monday-starting ISO week). Sample
#' records with a missing concentration or population served, or a PCR target other
#' than SARS-CoV-2, were excluded before aggregation. The number of contributing sites
#' grew substantially over the course of the program, from a handful of sites in 2020
#' to well over 100 per state by 2022-2023; see \code{n_sites} and \code{n_samples}.
#' See \code{data-raw/wastewater_data.R} for the full processing script.
#'
#' @format A data frame with 430 rows and 5 variables:
#'  \describe{
#'    \item{state}{US state name}
#'    \item{date}{date of the Monday starting the ISO week, in YYYY-MM-DD format}
#'    \item{conc}{population-weighted mean SARS-CoV-2 concentration across sites
#'      reporting that week (copies per person per day, flow- and
#'      population-normalized)}
#'    \item{n_sites}{number of distinct wastewater treatment sites contributing a
#'      sample that week}
#'    \item{n_samples}{number of individual sample records contributing that week}
#' }
#' @source \url{https://data.cdc.gov/resource/j9g8-acpt}
"wastewater_data"

#' Weekly reported COVID-19 cases for three US states
#'
#' Weekly new COVID-19 cases for California, New York, and Ohio, covering 20 January
#' 2020 to 27 March 2023, built by taking first differences of the cumulative case
#' counts reported by The New York Times and summing them into Monday-starting ISO
#' weeks (matching \code{\link{wastewater_data}}). Negative day-to-day differences
#' (occasional downward revisions in the source data) were floored at zero before
#' summing. The New York Times repository stopped updating in March 2023, which sets
#' the end of this series. See \code{data-raw/wastewater_data.R} for the full
#' processing script.
#'
#' @format A data frame with 485 rows and 3 variables:
#'  \describe{
#'    \item{state}{US state name}
#'    \item{date}{date of the Monday starting the ISO week, in YYYY-MM-DD format}
#'    \item{cases}{newly reported COVID-19 cases that week}
#' }
#' @source \url{https://github.com/nytimes/covid-19-data}
"covid_case_data"
