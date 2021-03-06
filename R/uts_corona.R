# require(dplyr) # for filter() and "%>%"
# require(tidyr) # for pivot_wider
# require(checkmate)
requireNamespace("dplyr", quietly = TRUE)   # for filter() and "%>%"
requireNamespace("tidyr", quietly = TRUE)   # for pivot_wider
requireNamespace("checkmate", quietly = TRUE) # assert_... functions


#' Utilities for corona data analysis and data
#'
#' Provide data in wide form and adapt column names
#'
#' @details The unquoted df data-variables are checked
#'
#' @param data A data frame to pivot
#' @return data frame
#' @import dplyr
#' @import tidyr
#' @import checkmate
#' @examples
#' # Corona data of "Germany"
#' uts_get_corona_data_wide(corona_data %>% dplyr::filter(Country == "Germany"))
#' @export
uts_get_corona_data_wide <- function(data) {

  # assert if names_data is subset of names of input data columns
  # => all columns exist for pivot_wider() and for select()
  names_data <- c("Country",  "Date", "Population", "Case_Type", "Cases",
                  "Daily_Cases", "Cases_100k", "Daily_Cases_100k",
                  "Mean_Daily_Cases", "Mean_Daily_Cases_100k")
  assert_names(names_data, subset.of = names(data))

  data %>%
    pivot_wider(names_from = .data$Case_Type,
                values_from = c(.data$Cases, .data$Daily_Cases,
                                .data$Cases_100k, .data$Daily_Cases_100k,
                                .data$Mean_Daily_Cases,
                                .data$Mean_Daily_Cases_100k)) %>%
    rename(Confirmed = .data$Cases_Confirmed,
           Deaths = .data$Cases_Deaths,
           Daily_Conf = .data$Daily_Cases_Confirmed,
           Daily_Deaths = .data$Daily_Cases_Deaths,
           Conf_100k = .data$Cases_100k_Confirmed,
           Deaths_100k = .data$Cases_100k_Deaths,
           Daily_Conf_100k = .data$Daily_Cases_100k_Confirmed,
           Daily_Deaths_100k = .data$Daily_Cases_100k_Deaths,
           Mean_Daily_Conf = .data$Mean_Daily_Cases_Confirmed,
           Mean_Daily_Deaths = .data$Mean_Daily_Cases_Deaths,
           Mean_Daily_Conf_100k = .data$Mean_Daily_Cases_100k_Confirmed,
           Mean_Daily_Deaths_100k = .data$Mean_Daily_Cases_100k_Deaths) %>%
    dplyr::select(.data$Country, .data$Population, .data$Date,
                  .data$Confirmed, .data$Daily_Conf, .data$Mean_Daily_Conf,
                  .data$Conf_100k, .data$Daily_Conf_100k,
                  .data$Mean_Daily_Conf_100k,
                  .data$Deaths, .data$Daily_Deaths, .data$Mean_Daily_Deaths,
                  .data$Deaths_100k, .data$Daily_Deaths_100k,
                  .data$Mean_Daily_Deaths_100k)
}

#' Reproduction number calculation
#'
#' Source: TU Ilmenau - GitHub
#'
#' @param new.cases Vector of daily cases data
#' @param profile  tbd
#' @param window tbd
#' @param delay tbd
#' @param conf.level tbd
#' @param pad.zeros tbd
#' @param min.denominator tbd
#' @param min.numerator tbd
#'
#' @details Here are further details
#'
#' ## subsection details and so on
#'
#' However, needs some text.
#'
#' ### sub-subsection details
#'
#' Also su-subsection needs some text.
#' @return data.frame with as many rows as new.cases
#'
#'     repronum = reproduction number
#'
#'     repronum.se = reproduction number standard error
#'
#'     ci.lower = confidence interval lower limit
#'
#'     ci.upper = confidence interval upper limit
#' @importFrom stats qnorm
#' @export
#' @references
#' **TU Ilmenau** *GitHub code source:* <https://github.com/Stochastik-TU-Ilmenau/COVID-19/blob/gh-pages/estimator.r>
#'
uts_repronum <- function(
  new.cases, # I
  profile, # w
  window = 1, # H
  delay = 0, # Delta
  conf.level = 0.95, # 1-alpha
  pad.zeros = FALSE,
  min.denominator = 5,
  min.numerator = 5
) {
  # pad zeros if desired
  if (pad.zeros) new.cases <- c(rep(0, length(profile) - 1), new.cases)

  # compute convolutions over h, tau and both, respectively
  sum.h.I <- as.numeric(stats::filter(new.cases, rep(1, window),
                                      method = "convolution", sides = 1))
  sum.tau.wI <- as.numeric(stats::filter(new.cases, c(0, profile),
                                         method = "convolution", sides = 1))
  sum.htau.wI <- as.numeric(stats::filter(sum.tau.wI, rep(1, window),
                                          method = "convolution", sides = 1))

  # estimators
  repronum <- ifelse(sum.h.I < min.numerator, NA, sum.h.I) / ifelse(sum.htau.wI < min.denominator, NA, sum.htau.wI)

  # standard errors
  repronum.se <- sqrt(repronum / sum.htau.wI)

  # shift by delay
  repronum <- c(repronum, rep(NA, delay))[(1:length(repronum)) + delay]
  repronum.se <- c(repronum.se,
                   rep(NA, delay))[(1:length(repronum.se)) + delay]

  # standard normal qunatile
  q <- qnorm(1 - (1 - conf.level) / 2)

  # return data.frame with as many rows as new.cases
  ret <- data.frame(
    repronum = repronum,
    repronum.se = repronum.se,
    ci.lower = repronum - q * repronum.se,
    ci.upper = repronum + q * repronum.se
  )
  if (pad.zeros) ret[-(1:(length(profile) - 1)),] else ret
}
