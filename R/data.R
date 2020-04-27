#' Daily confirmed cases of COVID-19 in China
#'
#' The case data has daily confirmed confirmed cases for different provinces in China from 16 January to 24 March 2020
#' from the dashboard maintained by Chinese Center for Disease Prevention and Control (CCDC). The CCDC dashboard
#' collates numbers of confirmed cases reported by national and local health commissions in each province in mainland
#' China, and Hong Kong SAR and Macau SAR. Confirmed cases are defined as suspected cases, who have epidemiological
#' links and/or clinical symptoms, and are detected with SARS-CoV-2 by PCR tests. However, in Hubei province,
#' clinically diagnosed cases were additionally included between 12 and 19 February.
#'
#' @format A data frame with 483 rows and 3 variables:
#' \describe{
#'    \item{date}{date, in YYYY-MM-DD format}
#'    \item{province}{name of province/region in China where cases occured}
#'    \item{cases}{number of daily cases reported of COVID-19}
#' }
#' @source \url{http://2019ncov.chinacdc.cn/2019-nCoV/}
"china_case_data"

#' Daily estimates of effective reporduction number from COVID-19 case data in China
#'
#' A dataset of effective reproduction number (Rt) estimates over time in different provinces of China.
#' @format A data frame with 496 rows and 5 variables:
#'  \describe{
#'    \item{date}{date, in YYYY-MM-DD format}
#'    \item{province}{name of province/region in China where cases occured}
#'    \item{r_mean}{mean effective reproduction number}
#'    \item{r_q2.5}{lower bound of the 95% credible interval for mean effective reproduction number}
#'    \item{r_q97.5}{upper bound of the 95% credible interval for mean effective reproduction number}
#' }
"china_rt_estimates"

#' Example exante movement data
#' @format A data frame
"exante_movement_data"
