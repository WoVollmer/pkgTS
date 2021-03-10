## data documentation for data/*.rda files

#' Corona Example / test dataset
#'
#' The datasets provide test data for the example which are
#' generated from the Johns Hopkins University data on GitHu.
#' The data does not include the lastet current case numbers.
#'
#' `corona_data` provides data from all countries,
#' `corona_data_sel` provides data from a few selected countries only
#'
#' see [Wolfgang's Corona Dashboard](https://wovollmer.github.io/github.io/#countries---table-overview)
#'
#' @docType data
#' @name data_corona
#' @format data.frame
#' @source Johns Hopkins University on GitHub
#' <https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series>
#' @keywords datasets
NULL

#' @rdname data_corona
#' @name corona_data
#' @usage data(corona_data)
#' @examples
#' head(corona_data)
#' uts_get_corona_data_wide(corona_data %>% dplyr::filter(Country == "Germany"))
NULL

#' @rdname data_corona
#' @name corona_data_sel
#' @usage data(corona_data_sel)
#' @examples
#' head(corona_data_sel)
#' unique(corona_data_sel$Country)
NULL


#' Climate Example / test dataset
#'
#' The climate datasets provide Temperature and Precipitation time series data
#' from Germany (DWD), Switzerland (meteoswiss) and atmospheric CO_2 concentration
#' time series data from Mauna Loa (esrl).
#'
#' `monthly_climate_basel` provides monthly Temperature and Precipitation
#' values (long format) from 2016 until 2020 without any NAs,
#' \cr
#' `monthly_climate_giessen` same as for Basel but with some NA values.
#'
#' `Basel_Temperature_ets_resid` provides ETS model data with fitted and
#' residual values.
#'
#' `monthly_co2_wide` provides monthly CO2 time series data (wide format)
#' from Mauna Loa from 1958 to 2020.
#'
#' @docType data
#' @name data_climate
#' @format data.frame
#' @source **Federal Office of Meteorology and Climatology MeteoSwiss**
#' <https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series>
#'
#' **DWD Archiv Monats- und Tageswerte**
#' <https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivtagmonat.html>
#'
#' **NOAA ESRL** <https://www.esrl.noaa.gov/gmd/ccgg/trends/global.html>
#' @keywords datasets
NULL

#' @rdname data_climate
#' @name monthly_climate_basel
#' @usage data(monthly_climate_basel)
#' @examples
#' # Temperature and Precipitation data
#' head(monthly_climate_basel)
#' tail(monthly_climate_basel)
#' uts_gen_yearly_seasonal_avg(monthly_climate_basel)
#'
NULL


#' @rdname data_climate
#' @name monthly_climate_giessen
#' @usage data(monthly_climate_giessen)
#' @examples
#' # Precipitation data have NAs
#' dplyr::slice(monthly_climate_giessen %>%
#'   dplyr::filter(Measure == "Precipitation"), 13:32)
#' uts_gen_yearly_seasonal_avg(monthly_climate_giessen)
#'
NULL

#' @rdname data_climate
#' @name Basel_Temperature_ets_resid
#' @usage data(Basel_Temperature_ets_resid)
#' @examples
#' # ETS model data
#' head(Basel_Temperature_ets_resid)
#' str(Basel_Temperature_ets_resid)
#' ggts_histo_forecast_resid(Basel_Temperature_ets_resid)
#'
NULL

#' @rdname data_climate
#' @name monthly_co2_wide
#' @usage data(monthly_co2_wide)
#' @examples
#' # CO_2 data wide format
#' monthly_co2_wide
NULL

