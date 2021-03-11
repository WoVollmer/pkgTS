requireNamespace("ggplot2", quietly = TRUE) # for ggplot(), geom_...()
requireNamespace("dplyr", quietly = TRUE)   # for filter() and "%>%"
# for pipe operator for "%>%" requireNamespace("magrittr", quietly = TRUE)
#    not needed, "Imports from other packages"
#    covered by
#          DESCRIPTION file: Imports: magrittr
#          file package.R NULL function:
#                #' @importFrom magrittr %>%   &
#                #' @export %>%
#          and documentation by #' @name pkg-imports
#          NAMESPACE file added by Roxygen:
#            export("%>%")
#            importFrom(magrittr,"%>%")
requireNamespace("tidyr", quietly = TRUE)   # for pivot_wider
requireNamespace("RColorBrewer", quietly = TRUE) # ColorBrewer palettes


#' Generate ggplot (Time-)Series Line Object with Rolling mean and Smooth line
#'
#' Calculates rolling mean with stats::filter(y, filter = rep(1/span, span)
#' and adding smooth line with geom_smooth(method = "loess").
#'
#' @param data data.frame with x (e.g. time line) and y data to be displayed,
#' e.g. ggplot_w_rol_mean(weather_long, Year_Month, Temperature, span = span).
#'   time : numeric string (also of data and Time classes) as index for
#'   column name.
#'   y : numeric string as index for y column name.
#' @param time data frame column for values of x axes.
#' @param y data frame column for values of y axes.
#' @param span integer width of the rolling window.
#' @details The data input must be a valid data.frame.
#'
#' data input format (e.g. Month in long format) \cr
#' `   Year_Month ...   count` \cr
#' `   <mth> ...   <dbl>` \cr
#' `1   1890 Jan ...     2.8` \cr
#' `2   1890 Feb ...    -1.7` \cr
#' `:` \cr
#' `n   2019 Dec ...     1.4` \cr
#' @return plot object of mode `plot
#' @seealso  [stats::filter] and [ggplot2::ggplot]
#' @examples
#' ggts_w_rol_mean(ggplot2::economics, date, uempmed, span = 7)
#' @keywords misc
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import checkmate
#' @export
ggts_w_rol_mean <- function(data, time = Year_Month, y = count, span) {
  # to address directly the column name embrace the arg (= col_name):  {{ arg }}
  #                                                            old: !!enquo(arg)
  # ggplot2 - same look and feel like => use aes({{ arg }}) or vars({{ arg }})
  #                                               old: aes_(x = substitute(arg))
  data <- data %>%
    mutate(rol_mean =
             stats::filter({{ y }}, filter = rep(1 / span, span))
    )
  # for getting "count": !!sym(y) or !!as.name(y)
  # print(tail(data))
  gg_plot <- ggplot(data, aes(x = {{ time }}, y = {{ y }})) +
    geom_smooth(aes(col = "Linear Reg"),
                method = "lm",
                size = 1, na.rm = TRUE
    ) +
    geom_smooth(aes(col = "Loess Reg"),
                method = "loess",
                size = 1, na.rm = TRUE
    ) +
    geom_line(aes(y = rol_mean, col = "Rolling Mean"), size = 1, na.rm = TRUE) +
    geom_line(aes(col = "Measured Values")) +
    # values: set colour values according ordering
    scale_colour_manual(
      limits =
        c(
          "Measured Values", "Rolling Mean", "Linear Reg",
          "Loess Reg"
        ),
      values = c("blue", "red", "green", "black")
    ) +
    theme(legend.position = "bottom") +
    labs(x = "Year", col = "") +
    ggtitle(paste(substitute(y), "over", substitute(time)),
            subtitle = paste("with Rolling Mean over", span, "measured values")
    )
  return(gg_plot)
}



#' @title Generate Time Series Decompression ggplot facet
#'
#' @description Generates from stlplus() output list components $Data and $time
#' a ggplot facet_wrap output over time for
#'  "raw" (=original data), "seasonal", "trend" and "remainder" components.
#'
#' @inherit ggts_w_rol_mean return
#'
#' @param data stlplus() output list.
#' @param ... Other arguments.
#' @examples
#' require(stlplus)
#' require(fpp3)
#' economics_tsbl <- ggplot2::economics %>%
#'  mutate(Year_Month = yearmonth(date)) %>%
#'  as_tsibble(index = Year_Month)
#' economics_tsbl %>% has_gaps() # check for time series gaps, fill with fill_gaps()
#' start_ts <- c(year(min(as.Date(economics_tsbl$Year_Month))),
#'   month(min(as.Date(economics_tsbl$Year_Month))))
#'
#' # convert to time-series object
#' economics_ts <- ts(economics_tsbl$uempmed, start = start_ts, frequency = 12)
#' # convert to tibble object
#' economics_stlplus <- stlplus(economics_ts, s.window = 27, t.window = 1201)
#' #
#' ggts_decomp(uts_stlplus_as_tibble(economics_stlplus))
#'
#' economics_stlplus <- stlplus(economics_ts, s.window = "per", t.window = 1201)
#' ggts_decomp(uts_stlplus_as_tibble(economics_stlplus))
#' @keywords misc
#' @seealso [uts_stlplus_as_tibble] and [stlplus::stlplus]
#'
#' @export
ggts_decomp <- function(data, ...) {
  # #' @seealso \code{\link[stlplus]{stlplus}} and `[stlplus::stlplus]` and `[uts_stlplus_as_tibble]`.
  data_long <- data %>%
    mutate(trend_for_raw = Trend) %>% # to add trend line in facet Raw only
    rename("Data with Trend" = Raw) %>%
    pivot_longer(
      cols = c("Data with Trend", "Seasonal", "Trend", "Remainder"),
      names_to = "Decompressed",
      values_to = "value"
    ) %>%
    mutate(Decompressed = factor(Decompressed,
      levels = c(
        "Data with Trend", "Seasonal",
        "Trend", "Remainder"
      )
    ))

  # 'trend_for_raw' and 'NA_replace' only to be plotted in 'Data with Trend' facet
  data_long$trend_for_raw[data_long$Decompressed != "Data with Trend"] <- NA
  data_long$NA_replace[data_long$Decompressed != "Data with Trend"] <- NA

  # provide mean values for horizontal line
  data_long  <- data_long %>%
    mutate(
      horiz_line =
        case_when(
          Decompressed == "Data with Trend" ~
            mean(data$Raw, na.rm = TRUE),
          Decompressed == "Seasonal" ~
            mean(data$Seasonal, na.rm = TRUE),
          Decompressed == "Trend" ~
            mean(data$Trend, na.rm = TRUE),
          Decompressed == "Remainder" ~
            mean(data$Remainder, na.rm = TRUE)
        )
  )

  x_segment <- data$Year_Month[1] - (data$Year_Month[6] - data$Year_Month[1])
  y_rem_min <- min(data$Remainder, na.rm = TRUE)
  y_rem_max <- max(data$Remainder, na.rm = TRUE)

  data_long <- data_long %>%
    mutate(
      y_seg_min = horiz_line - abs(y_rem_min),
      y_seg_max = horiz_line + abs(y_rem_max)
  )


  gg_plot <- ggplot(data_long, aes(Year_Month, value, col = Decompressed)) +
    facet_wrap(~Decompressed, ncol = 1, scales = "free", strip.position = "right") +
    geom_line(aes(y = horiz_line), na.rm = TRUE, linetype = "dashed", size = 1) +
    geom_line(aes(y = trend_for_raw),
      na.rm = TRUE, col = "cyan",
      linetype = "solid", size = 0.8
    ) +
    geom_point(aes(y = NA_replace), na.rm = TRUE, col = "green", size = 0.5) +
    geom_line() + # TRUE rows w/ NA are silently removed, no warning
    # FALSE (default), warning indicates # of missing values = NA values,
    # Raw data, replaced "only" in count column
    geom_segment(aes(x = x_segment, y = y_seg_min, xend = x_segment, yend = y_seg_max),
      colour = "darkgrey", size = 2
    ) +
    theme(legend.position = "none") +
    labs(x = "Year", col = "") +
    ggtitle("Time Series Decompression w/ Mean Horiz. Lines",
      subtitle = "with Function stlplus()"
    )

  return(gg_plot)
}


#' Provides Monthly data plot (e.g. Temperature, Precipitation, ...)
#'
#' Monthly plot will be provided wit loess smooth line (no facetting)
#'
#' @inheritParams ggts_w_rol_mean
#' @param data data frame with monthly data (long format)
#' @param season col aestetic for points and loess smooth
#' @inherit ggts_w_rol_mean return
#'
#' @examples
#' ggts_season_w_smooth(monthly_climate_basel)  +
#'   ggplot2::facet_wrap(~Measure, ncol = 1, scales = "free")
#' @export
ggts_season_w_smooth <- function(data, time = Year_Month, y = count,
                                 season = Month) {
    x_axis_theme <- element_text(size = 14)

    graph <- ggplot(data, aes({{ time }}, {{ y }}, col = {{ season }})) +
    geom_point(na.rm = TRUE) +
    geom_smooth(method = "loess", size = 0.5, na.rm = TRUE) +
    labs(x = "Year") +
    # scale_colour_hue(): to get "cold" colours in winter, "warm" in summer
    scale_colour_hue(h.start = -140, c = 200, l = 60) +  # h.start = -140,
    ggtitle("Monthly Data with Local Polynomial Regression Fitting") +
    theme(axis.title.x = x_axis_theme)

  return(graph)
}

#' Provides Season (Winter, Spring, Summer, Fall) line and smooth plot
#' in one commmon diagram
#'
#' @param data data frame with yearly data (long format) with average values
#' for the seasons.
#' @examples
#' ggts_season(uts_gen_yearly_seasonal_avg(monthly_climate_basel))  +
#'   ggplot2::facet_wrap(~Measure, ncol = 1, scales = "free")
#' @export
ggts_season <- function(data) {
  x_axis_theme <- element_text(size = 14)

  data_season <- data %>%
    rename(
      Winter = Winter_avg, Spring = Spring_avg,
      Summer = Summer_avg, Fall = Fall_avg
    ) %>%
    pivot_longer(
      cols = c(Winter:Fall),
      names_to = c("Season"),
      values_to = "count"
    )

  graph <-
    ggplot(data_season, aes(x = Year)) +
    # geom_point(col = "blue", shape = 20, na.rm = TRUE) +
    geom_line(aes(y = count, col = Season), linetype = "solid", na.rm = TRUE) +
    geom_smooth(aes(y = count, col = Season), method = "loess", na.rm = TRUE) +
    # limits: ordering of colours, values: set colour values according ordering
    scale_colour_manual(
      limits = c("Winter", "Spring", "Summer", "Fall"),
      values = c("blue", "green", "orange", "brown")
    ) +
    theme(legend.position = "bottom") +
    labs(x = "Year", y = "Value") +
    ggtitle("Average Season Data (w/ Loess Regression Lines)",
      subtitle =
        "Winter (DJF, DecJanFeb), Spring (MAM), Summer (JJA), Fall (SON)"
    ) +
    theme(axis.title.x = x_axis_theme)


  return(graph)
}

#' Year over Month plot
#'
#' @inheritParams ggts_w_rol_mean
#' @param data data frame with yearly data (long format) with average values
#' for the seasons.
#' @param period group and col aestetic for points and lines
#' @inherit ggts_w_rol_mean return
#' @examples
#' plot_monthly <- ggts_year_over_month(monthly_climate_basel %>%
#'   dplyr::filter(Measure == "Temperature"), period = Year)
#' plot_monthly
#' plot_monthly + ggplot2::coord_polar("x", start = pi)
#' # remark: coord_polar doesn't support free scales, filter Measure beforehand
#'
#' ggts_year_over_month(monthly_climate_basel, period = Year) +
#'   ggplot2::facet_wrap(~Measure, ncol = 1, scales = "free_y")
#' @export
ggts_year_over_month <- function(data, time = Month, y = count, period = Period) {
  x_axis_theme <- element_text(size = 14)
  col_scheme <- "YlOrRd"

  gg_plot <- ggplot(data, aes({{ time }}, {{ y }},
                              group = {{ period }},
                              col = {{ period }} )) +
    geom_point(shape = 5, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    labs(x = "Month") +
    ggtitle(paste0(
      "Monthly Variations of ", substitute(col),
      "-counts over Month"
    )
    # subtitle = paste(
    #   min(data$Period),
    #   " - ", max(data$Period) )
    ) +
    theme(axis.title.x = x_axis_theme) +
    scale_colour_distiller(palette = col_scheme, direction = 1)
  # scheme for Temp: "YlOrRd"; for Precip: "YlGnBu"
  return(gg_plot)
}

#' Function provides yearly/ span*yearly Temperature and Precipitation
#'
#' @param data tibble / tsibble generated with augment(mable).
#' The mable is outcome of  ETS or ARIMa model fit.
#' @inherit ggts_w_rol_mean return
#'
#' @examples
#' ggts_histo_forecast_resid(Basel_Temperature_ets_resid)
#' @seealso `[fabletools::augment_mdl_df]`
#' @export
ggts_histo_forecast_resid <- function(data) {
  # make a histogram of the forecast errors:
  mybinwidth <- IQR(data$.resid) / 4 # set to 1/4 of quartile range
  mysd <- sd(data$.resid)
  mymin <- min(data$.resid) - mysd * 3 # beforehand *5
  mymax <- max(data$.resid) + mysd * 3
  # generate normally distributed data with mean 0 and standard deviation mysd
  # and with same data length as data$.resid
  n <- nrow(data) - 1
  x <- seq(mymin, mymax, by = (mymax - mymin) / n)
  normal_distr <- tibble::tibble(x = x, PDF = dnorm(x, mean = 0, sd = mysd))

  gg_hist <- ggplot(data, aes(.resid, ..density..)) +
    ggtitle("Histogram of Forecast Residuals",
      subtitle = "with overlaid normal curve"
    ) +
    xlab("Forecast Residuals") +
    geom_histogram(
      binwidth = mybinwidth,
      col = "black", fill = "red", alpha = 0.7
    ) +
    geom_line(aes(x = normal_distr$x, y = normal_distr$PDF), col = "blue")


  # hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  # myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  # points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)

  return(gg_hist)
}
