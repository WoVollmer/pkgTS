#' @title New package provides function for **Corona** data analysis
#'
#' @description Package creates documentation with roxygen2.
#'
#' **Markdown** is running also without `@md`*-tag*.
#'
#' @details Details are still not outlined.
#'
#' For an overview of R procedures and testthat files start with the vignettes:
#' browseVignettes(package = "newpkg"). However, so far no vignette is written.
#'
#' @seealso Different links w/ and w/o backticks:
#'
#' Main plot functions based on [ggplot2::ggplot] for
#' [`ggplot2::ggplot`] time series data:
#'
#' [ggts_cum_daily], [ggts_trend_facet()] and [ggts_conf_deaths_facet]
#'
#' [`ggts_cum_daily`], [`ggts_trend_facet()`] and [`ggts_conf_deaths_facet`]
#'
#' and utility functions for time series:
#'
#' [uts_get_corona_data_wide], [uts_repronum]
#'
#' @docType package
#' @name newpkg_package
#' @author Wolfgang Vollmer \email{wo.vollmer@@online.de}
#' @keywords package
NULL

## documentation for imported functions requiring export

#' @importFrom magrittr %>%
#' @export %>%
NULL

# If %>% is imported => can be used in the code of the package
#   but NOT in the examples ! => ImportFrom plus @export
# see
# https://stackoverflow.com/questions/56509068/r-package-fails-devtoolscheck-because-could-not-find-function-even-though-t
# see https://github.com/rstudio/DT/blob/master/R/package.R


#' Imports from other packages
#'
#' Objects imported from other packages
#'
#' @details These objects are imported from other packages.
#' Follow the links to their documentation.
#' * magrittr forward-pipe operator: [`magrittr::%>%`]
#'
#' @name pkg-imports
#' @aliases %>%
#' @docType import
NULL

