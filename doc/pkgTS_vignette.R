## ---- include = FALSE---------------------------------------------------------
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


## ---- fig.width = 7, fig.asp = 0.618, fig.show='hold'-------------------------

ggts_cum_daily(corona_data, country = "Germany", weeks = 6)


