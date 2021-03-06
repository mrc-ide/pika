% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{exante_movement_data}
\alias{exante_movement_data}
\title{Daily within-city movement data for different regions in China}
\format{
A data frame with 672 rows and 3 variables:
 \describe{
   \item{date}{date, in YYYY-MM-DD format}
   \item{province}{name of province/region in China where cases occured}
   \item{movement}{daily population-weighted with-in city movement index}
}
}
\usage{
exante_movement_data
}
\description{
The daily within-city movement data, used as a proxy for economic activity, is available from 1 January to
24 March 2020 for major metropolitan cities within each province in mainland China, Hong Kong SAR, and Macau SAR.
These data, provided by Exante Data Inc, measured travel activity relative to the 2019 average (excluding Lunar
New Year). The underlying data are based on near real-time people movement statistics from Baidu.
}
\keyword{datasets}
