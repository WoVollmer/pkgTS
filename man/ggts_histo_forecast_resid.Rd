% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggts_climate.R
\name{ggts_histo_forecast_resid}
\alias{ggts_histo_forecast_resid}
\title{Function provides yearly/ span*yearly Temperature and Precipitation}
\usage{
ggts_histo_forecast_resid(data)
}
\arguments{
\item{data}{tibble / tsibble generated with augment(mable).
The mable is outcome of  ETS or ARIMa model fit.}
}
\value{
plot object of mode `plot
}
\description{
Function provides yearly/ span*yearly Temperature and Precipitation
}
\examples{
ggts_histo_forecast_resid(Basel_Temperature_ets_resid)
}
\seealso{
\verb{[fabletools::augment_mdl_df]}
}
