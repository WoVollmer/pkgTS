## data documentation for data/*.rda files

#' Example / test dataset
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
#' @name pkg_data
#' @format data.frame
#' @source Johns Hopkins University on GitHub
#' <https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series>
#' @keywords datasets
NULL

#' @rdname pkg_data
#' @name corona_data
#' @usage data(corona_data)
#' @examples
#' head(corona_data)
#' uts_get_corona_data_wide(corona_data %>% dplyr::filter(Country == "Germany"))
NULL

#' @rdname pkg_data
#' @name corona_data_sel
#' @usage data(corona_data_sel)
#' @examples
#' head(corona_data_sel)
#' unique(corona_data_sel$Country)
NULL
