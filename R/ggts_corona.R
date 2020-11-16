# library(ggplot2)  # for ggplot(), geom_...()
# library(dplyr) # for filter() and "%>%"
# library(magrittr) # for "%>%"  but "Imports from other packages"
#    covered by
#          DESCRIPTION file: Imports: magrittr
#          file package.R NULL function:
#                #' @importFrom magrittr %>%   &  #' @export %>%     NULL
#                #' @name pkg-imports
#          NAMESPACE file (gen . by Roxygen): export("%>%") & importFrom(magrittr,"%>%")
# require(tidyr) # for pivot_wider
requireNamespace("ggplot2", quietly = TRUE) # for ggplot(), geom_...()
requireNamespace("dplyr", quietly = TRUE)   # for filter() and "%>%"
requireNamespace("tidyr", quietly = TRUE)   # for pivot_wider


# R CMD check issue: note: no-visible-binding-for-global-variable
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
# #' @importFrom rlang .data  # for tidyverse use .data pronoun
# => neeeds @importFrom
#
# #' @importFrom utils head   # for usage of utlis::head() otherwise R CMD check note: no-visible-binding...

# library(patchwork) # package for composing plots
# library(highcharter) # package for highchart plots
# library(dygraphs) # package for dygraph plots
requireNamespace("patchwork", quietly = TRUE) # for ggplot(), geom_...()
requireNamespace("highcharter", quietly = TRUE)   # for filter() and "%>%"
requireNamespace("dygraphs", quietly = TRUE)   # for pivot_wider

#' Cumulative and daily data trend plot
#'
#' Provide **trend plot** for *cumulative* and *daily* cases with
#' facets of `vars_1 = Case_Type`
#'
#' @param data A data frame
#' @param y_cum Unquoted `column name` of the data frame's cumulative cases
#' @param y_daily Unquoted `column name` of the data frame's daily cases
#' @param daily_mean Unquoted `column name` of the data frame's daily rolling mean data
#' @param country Unquoted `column name` of the data frame's countries
#' @param span Numeric, span used for rolling mean calculation
#' @param weeks Numeric, number of time range weeks weeks for the daily data,
#'   dates are provided in column `default = Date`
#' @param ... Variable pass through to [ggts_trend_facet()].
#' Unquoted `column name` of the data frame's dates.
#'
#' @return plot object of mode "`plot`"
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import patchwork
#' @export
#' @seealso [ggts_trend_facet], [ggts_conf_deaths_facet] and [ggplot2::ggplot]
#'
#' @examples
#' # Corona data of "Germany")
#' ggts_cum_daily(corona_data, country = "Germany", weeks = 6)
ggts_cum_daily <- function(data,
                           y_cum = .data$Cases,
                           y_daily = .data$Daily_Cases,
                           daily_mean = .data$Daily_Cases_Mean,
                           country,
                           span = 7,
                           weeks = 12, ...) {
  data <- data %>% dplyr::filter(Country == country)

  plot_cum_cases <- ggts_trend_facet(data, y = {{ y_cum }}) +
    labs(title = paste(country, "- cumulative Cases (since Jan 2020)"))

  last_date <- max(data$Date)
  data <- data %>% dplyr::filter(Date >= last_date - 7 * weeks + 1)

  plot_daily_cases <- ggts_trend_facet(data, y = {{ y_daily }}) +
    geom_line(aes(y = {{ daily_mean }}, col = "Rolling Mean"),
              size = 1, na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "14 days") +
    labs(title = paste(country, "- Daily Cases (past", weeks, "weeks)"),
         subtitle = paste0("with Rolling Mean of past ", span, " days"))

  plot_cum_cases + plot_daily_cases
  # + plot_annotation(tag_levels = "A", title = "title annot")
}

#' Trend facet plot for each case type
#'
#' Provide trend facet plot for each case type (e.g. Confirmed and Deaths)
#'
#' @param data A data frame
#' @param x Unquoted `column name` of the data frame's dates
#' @param y Unquoted `column name` of the data frame's cases
#' @param vars_1 Unquoted `column name` of the data frame's case types
#' @return plot object of mode "`plot`"
#' @export
#' @seealso [ggts_cum_daily]
#' @examples
#' # Corona data of "Germany" - plot cumulative cases for 'Confirmed' and
#' # 'Deaths' and 'Confirmed'
#' ggts_trend_facet(corona_data %>% dplyr::filter(Country == "Germany"))
#' ggts_trend_facet(corona_data %>% dplyr::filter(Country == "Germany"), y = Cases_100k)
ggts_trend_facet <- function(data,
                             x = .data$Date,
                             y = .data$Cases,
                             vars_1 = .data$Case_Type) {
  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" #
  p <- ggplot2::ggplot(data, aes({{ x }}, {{ y }}, col = {{ vars_1 }})) +
    facet_wrap(vars({{ vars_1 }}), ncol = 1, scales = "free_y",
		strip.position = "left") +
    geom_point(size = 1, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    scale_x_date(date_labels = "%b %d", date_breaks = "28 days") +
    theme(legend.position = "none")  +
    theme(plot.title = element_text(size = 10))
  # scale_colour_distiller(palette = col_scheme, direction = 1) +
  # scale_colour_brewer(palette = col_scheme, direction = 1) +
  # scale_color_discrete(c("blue",  "green", "red")) +
  p
}


#' Trend facet plot for each case type and each country
#'
#' grid plot Confirmed / Death for each country
#'
#' @inheritParams ggts_trend_facet
#' @inherit ggts_trend_facet return
#' @param vars_2 Unquoted `column name` of the data frame's countries
#' @export
#' @seealso [ggts_trend_facet]
#'
#' @examples
#' # Corona data of "Germany", "Italy", "United States of America")
#' ggts_conf_deaths_facet(corona_data_sel)
ggts_conf_deaths_facet <- function(data,
                                   x = .data$Date,
                                   y = .data$Cases,
                                   vars_1 = .data$Case_Type,
                                   vars_2 = .data$Country) {

  # col_scheme <- "Set1" # "RdYlGn" #"YlOrRd" #"Oranges" # "YlGnBu" #

  # see dplyr vignette: Programming with dplyr
  # globalVariables(c("Date", "Cases", "Case_Type", "Country"))
  # still: no visible binding for global variable 'Date' ...
  # assign NULL to each undefined column name
  #    => no longer search for global variable => note issue with R CMD check
  # Date <- Cases <- Case_Type <- Country <- NULL
  #             => defined after it is used as arg

  ggplot2::ggplot(data, aes({{ x }}, {{ y }}, col = {{ vars_1 }})) +
    facet_grid(vars({{ vars_2 }}, {{ vars_1 }}), scales = "free_y") +
    geom_point(size = 1.5, na.rm = TRUE) +
    geom_line(size = 1, na.rm = TRUE) +
    theme(legend.position = "none")  +
    labs(y = "") +
    scale_x_date(date_labels = "%b %d", date_breaks = "7 days") +
    # scale_colour_distiller(palette = col_scheme, direction = 1) +
    # scale_colour_brewer(palette = col_scheme, direction = 1) +
    # scale_color_discrete(c("blue",  "green", "red")) +
    ggtitle("Confirmed and Death - Daily Cases (past 3 weeks)")
}

### log scale #############################

#' plot countries on log10scale
#'
#' @inheritParams ggts_conf_deaths_facet
#' @inherit ggts_trend_facet return
#' @export
#' @examples
#' # Corona data of "Germany", "Italy", "United States of America")
#' ggts_logscale(corona_data_sel)
ggts_logscale <- function(data,
                          x = .data$Date,
                          y = .data$Cases,
                          vars_1 = .data$Country,
                          vars_2 = .data$Case_Type) {
  # gg_plot <-
  ggplot2::ggplot(data, aes({{ x }}, y= log10({{ y }}), col = {{ vars_1 }})) +
    labs(x = "Date", y = substitute(y),
         title =
           "Virus Spread on log10 scale - World and selected Countries") +
    geom_line() +
    # geom_smooth(method="loess", aes(col = {{ vars_1 }}), lty = "dashed", se=FALSE) +
    theme(legend.position = "bottom") +
    facet_wrap(vars({{ vars_2 }}), ncol = 2, scales = "free_y",
               strip.position = "left")
}


### highchart #############################

#' highcharter World Map plot
#'
#' @param data A data.frame object with data to chart. Code region and value are required.
#' @param value A string value with the name of the variable to chart.
#' @param title A string value with the title of the chart.
#'
#' @return highchart plot
#' @export
#' @import highcharter
#'
#' @seealso
#' **Manual pages for highcharter**
#'  <https://rdrr.io/cran/highcharter/man/hc_add_series_map.html>
#'
#'  <https://rdrr.io/cran/highcharter/man/hc_xAxis.html>
#'
#' **Highcharts** *CONFIGURATION OPTIONS*
#'  <https://api.highcharts.com/highcharts/title>
#' @examples
#' last_date <- max(corona_data$Date)
#' data <- corona_data %>%
#'   dplyr::filter(Date == last_date & Case_Type == "Confirmed" &
#'                 Country != "World")
#'
#' data(worldgeojson, package = "highcharter")
#' hc_world_map_plot(data, value = "Population",  title = "World Population")
#'
hc_world_map_plot <- function(data, value, title = "Text") {
  # globalVariables(c("worldgeojson"))
  # worldgeojson <- NULL => error: map is not a list
  # worldgeojson <- list(NULL) =>
  highchart() %>%
    hc_add_series_map(worldgeojson,
                      df = data,
                      value = value,
                      joinBy = c('name', 'Country'))  %>%
    # name checks `%in%`(x = value, table = names(data))
    #hc_colors(c("darkorange", "darkgray")) %>%
    hc_colorAxis(stops = color_stops()) %>%
    hc_title(text = title) %>%
    hc_yAxis(title = list(text = ("cumulative Cases")))
}

### bar chart #############################


#' Bar Chart plot with highchart()
#'
#' Visualization with top x country series bar chart
#'
#' @inheritParams hc_world_map_plot
#' @param x A string value with the column name for the bar names,
#'    default = "Country".
#' @param y A string value with the column name for the bar data values.
#' @param n numeric, number of bars in chart
#'
#' @inherit hc_world_map_plot seealso return
#' @seealso
#' **Create a highchart object**
#' <https://rdrr.io/cran/highcharter/man/hchart.html>
#'
#' **Highcharts** *series.bar* and *aesthetic mappings*
#' <https://api.highcharts.com/highcharts/series.bar>
#' <https://rdrr.io/cran/highcharter/man/hcaes.html>
#'
#' @examples
#' last_date <- max(corona_data$Date)
#' data <- corona_data %>%
#'   dplyr::filter(Date == last_date & Case_Type == "Confirmed" & Country != "World")
#' hc_bar_chart_country(data, y = "Population",  title = "World Population")
#' @export
hc_bar_chart_country <- function(data, x = "Country", y, title = "Text", n = 15) {
  # #' @importFrom utils head # taken out since utils now in DESCRIPTION
  # #' @importFrom rlang .data
  #     rename(col_name = .data$key) %>%
  #     mutate(col_name = as.character(.data$col_name))
  data %>% # ungroup() %>%
    slice_max(order_by = .data[[y]], n = n) %>%
    rename(x = .data[[x]],
           y = .data[[y]]) %>%
    # hchart() does not work with {{ y }} or substitute(), ....
    #      => fixed colname to be used and hc_yAxis() needed, otherwise "y"
    hchart("bar", hcaes(x = x,  y = y)) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text = x)) %>%
    hc_yAxis(title = list(text = y)) %>%
    hc_add_theme(hc_theme_sandsignika())

}


#' dygraph plot for Daily Confirmed and Death Cases
#'
#' dygraph interactive plot for xts time series objects:
#' input must be a named list or data frame, where the first element/column
#' provides x-axis values and all subsequent elements/columns provide one or
#' more series of y-values.
#'
#' @param data_xts tbd
#' @param country_select tbd
#' @param last_date tbd
#' @param span tbd
#' @param weeks tbd
#'
#' @return tbd
#' @export
#' @import dygraphs
#'
#' @references <https://rstudio.github.io/dygraphs/gallery-series-highlighting.html>
#'
plot_dygraph_daily <-
  function(data_xts, country_select, last_date, span = 7, weeks = 12) {
    dygraph(data_xts,
            main = paste0(country_select,
                          " - Daily Cases with Rolling Mean of past ",
                          span, " days")) %>%
      dyAxis("y", label = "Daily Confirmed Cases") %>%
      dyAxis("y2", label = "Daily Death Cases",  independentTicks = TRUE) %>%
      dyLegend(width = 400) %>%
      dySeries("Daily_Conf",
               drawPoints = TRUE, pointSize = 3, pointShape = "circle",
               color = "tomato") %>%
      dySeries("Daily_Conf_Mean", drawPoints = FALSE,  color = "red") %>%
      dySeries("Daily_Deaths",
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle",
               color = "turquoise", axis = "y2") %>%
      dySeries("Daily_Deaths_Mean", drawPoints = FALSE,
               color = "blue", axis = "y2") %>%
      dyRangeSelector(dateWindow =
                        c(as.character(last_date - weeks * 7), as.character(last_date)))
  }

#' dygraph plot for Daily Confirmed Cases and Reproduction Number w/ CI
#'
#' @inheritParams plot_dygraph_daily
#'
#' @inherit plot_dygraph_daily references return
#' @export
#'
plot_dygraph_daily_repro <-
  function(data_xts, country_select, last_date, span = 7, weeks = 12) {
    dygraph(data_xts,
            main =  paste0(country_select,
            " - Reproduction Number based on Daily Confirmed Cases (",
            span, "-day window)")) %>%
      dyAxis("y", label = "Reproduction Number w/ Confidence Interval") %>%
      dyAxis("y2", label = "Daily Confirmed Cases",
             independentTicks = TRUE) %>%
      dyLegend(width = 400) %>%
      dySeries(c("ci.lower","Repro_number", "ci.upper"),
               color = "black") %>%
      dyLimit(1, "Repro_number = 1",
              strokePattern = "dashed", color = "black") %>%
      dySeries("Daily_Conf",
               drawPoints = TRUE, pointSize = 3, pointShape = "triangle",
               color = "tomato", axis = "y2") %>%
      dySeries("Daily_Conf_Mean", drawPoints = FALSE,
               color = "red", axis = "y2") %>%
      dyRangeSelector(dateWindow =
                        c(as.character(last_date - weeks * 7), as.character(last_date)))
  }
