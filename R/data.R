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
#'  \describe{
#'    \item{date}{date, in YYYY-MM-DD format}
#'    \item{province}{name of province/region in China where cases occured}
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
#'    \item{province}{name of province/region in China where cases occured}
#'    \item{movement}{daily population-weighted with-in city movement index}
#' }
"exante_movement_data"
