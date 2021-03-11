## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(fig.width = 7, fig.asp = 0.618)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pkgTS)

## ---- echo = TRUE-------------------------------------------------------------

dir("./../R/")


## -----------------------------------------------------------------------------

tail(corona_data %>% dplyr::filter(Country == "Germany"))



## -----------------------------------------------------------------------------

tail(uts_get_corona_data_wide(
  corona_data %>% dplyr::filter(Country == "Germany")))


## ---- fig.show='hold'---------------------------------------------------------

ggts_cum_daily(corona_data, country = "Germany", weeks = 6)


## -----------------------------------------------------------------------------

head(monthly_climate_basel)

tail(monthly_climate_basel)


## -----------------------------------------------------------------------------

tail(monthly_co2_wide)


## ---- fig.show='hold'---------------------------------------------------------

plot_monthly <- ggts_year_over_month(monthly_climate_basel %>%
                                       dplyr::filter(Measure == "Temperature"), period = Year)

plot_monthly
plot_monthly + ggplot2::coord_polar("x", start = pi)

ggts_year_over_month(monthly_climate_basel, period = Year) +
  ggplot2::facet_wrap(~Measure, ncol = 1, scales = "free_y")

