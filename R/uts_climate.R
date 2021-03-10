requireNamespace("dplyr", quietly = TRUE)   # for filter() and "%>%"
requireNamespace("tidyr", quietly = TRUE)   # for pivot_wider
requireNamespace("tsibble", quietly = TRUE)   # for indexing, ...
requireNamespace("checkmate", quietly = TRUE) # assert_... functions
requireNamespace("fpp3", quietly = TRUE)    # library call in examples
requireNamespace("stlplus", quietly = TRUE) #  functions

#' @title Utilities for climate analysis and data reforming
#'
#' @description "Widens" monthly long form data form and calculates
#' * yearly average counts for Jan:Dec and
#' * seasonal average counts mean data/month
#'   + seasons are Dec-Feb (DJF), Mar-May (MAM), June-Aug (JJA), Sept-Nov (SON)
#'   + Winter: year refers to January (realized by lag()).
#' * note for Precipitation: to be multiplied by 12 if mm/year is needed
#' @param data A data frame with monthly "count" in long format
#' @param season Logical. If TRUE, the default, seasonal mean will be calculated
#' @details The input data frame must have the column names
#'
#' `City, Measure, Year_Month, Year` and `count`
#'
#' and can be also a tibble or tsibble.
#'
#' @return data frame and tibble (not a tsibble), e.g.: \cr
#' `A tibble: 10 x 8`
#' `Groups:   City, Measure [2]`  # here: Measure = Temperature and Precipitation
#' `City  Measure Year   Winter_avg Spring_avg Summer_avg Fall_avg Year_avg` \cr
#' `<chr> <fct>   <dbl>  <dbl>      <dbl>      <dbl>      <dbl>    <dbl>`
#'
#' @import dplyr
#' @import tidyr
#' @import checkmate
#' @import tsibble
#' @examples
#' # Temperature and Precipitation data of "Basel" w/o NAs rsp. Giessen w/ NAs
#' uts_gen_yearly_seasonal_avg(monthly_climate_basel)
#' uts_gen_yearly_seasonal_avg(monthly_climate_giessen)  # w/ NAs
#' # note: NAs of Winter_avg does not impact Year_avg if this
#' # NA is caused by December value of previous year
#' @export
uts_gen_yearly_seasonal_avg <- function(data, season = TRUE) {

  # assert if names_data is subset of names of input data columns
  # => all columns exist for pivot_wider() and for select()
  names_data <- c("City",  "Measure", "Year_Month", "Year", "count")
  assert_names(names_data, subset.of = names(data))

# mean of year for all years grouped by (City, Measure, Year)
  data_yearly <- data %>%
  #  index_by(Year = ~ year(.)) %>%
  # data_yearly <- as_tsibble(data, index =  Year, key = Measure) %>%
    tibble::as_tibble() %>%
    group_by(City, Measure, Year) %>%
    summarise(Year_avg = mean(count))  # na.rm = TRUE not feasible for mean(year)
  # slice(data_yearly, 1:6)

  if (!tsibble::is_tsibble(data)) {
    data <- data %>% tsibble::as_tsibble(index = Year_Month, key = Measure)
  }

  if (season) {
    # mean of season for all years grouped by (City, Measure, Year_Season)
    mean_season <- data %>%
      group_by(City, Measure) %>%
      # important: grouping before call lag(), otherwise rolling over Measure
      mutate(count_lag = lag(count)) %>%
      index_by(Year_Season = ~ yearquarter(.)) %>%
      as_tibble() %>%
      group_by(City, Measure, Year_Season) %>%
      summarise(Season_avg = mean(count_lag)) %>%
      mutate(Year = lubridate::year(Year_Season),
             Season = lubridate::quarter(Year_Season))
    # slice(mean_season, 1:6)

    mean_season_wide <- mean_season %>%
      pivot_wider(id_cols = c(City, Measure, Year),
                  names_from = Season,
                  values_from = Season_avg) %>%
      rename(Winter_avg = '1', Spring_avg = '2', Summer_avg = '3', Fall_avg = '4')
    # slice(mean_season_wide, 1:6)

    data_yearly <- inner_join(mean_season_wide, data_yearly)
  }
  return(data_yearly)
}

#' Check monthly long format data for gaps and fill with NAs if data are missing
#'
#' Time series objects don't allow missing years or months.
#' Check and fill missing data with NAs w/ `{tsibble}` functions and provide
#' output as tsibble with generated Year_Month as time index.
#'
#' @param data A data frame with monthly "count" in long format and
#' separate columns "Year" and "Month"
#' @param key Variable(s) that uniquely determine time indices.
#' NULL (dafault) for empty key. Required, if multiple time indices exist
#' (e.g. key = "Measure" if Temperature and Precipitation data exist)
#'
#' @details Time series objects don't allow gaps in time (missing years or months).
#' With {tsibble} functions check and fill gaps in time and add NA count values
#' and provide output in wide Month format (Year Temp_Precip Jan Feb .... Dec)
#'
#' data input format (Month in long format) \cr
#' `Year Month Temp_Precip   count` \cr
#' `<dbl> <dbl> <chr>         <dbl>` \cr
#' `1  1887     1 Temperature    NA` \cr
#' `2  1887     1 Precipitation   4` \cr
#' `3  1887     2 Temperature    NA` \cr
#' `:` \cr
#' `5  2019    12 Temperature    4.55` \cr
#' `6  2019    12 Precipitation 30.4` \cr
#'
#'
#' data output format (Month in wide format) \cr
#' `Year Temp_Precip   Jan   Feb  ... Dec` \cr
#' `<dbl> <fct>       <dbl> <dbl> ... <dbl>` \cr
#' `1  1887 Temperature NA    NA  ... NA` \cr
#' `:` \cr
#' `3  1889 Temperature -3.1 -2.09 ...-1.01` \cr
#'
#' @return data frame and tsibble, e.g.: \cr
#' `A tsibble: 754 x 4` \cr
#' `Year_Month  Year Month count` \cr
#' `<mth> <dbl> <fct> <dbl>` \cr
#' @examples
#' data <- monthly_climate_basel %>%
#'   dplyr::select(City, Measure, Year, Month, count)
#' uts_data_check_and_fill_w_na(data, key = "Measure")
#'
#' # delete all "2017" and "Feb" rows and fill with NAs
#' data <- monthly_climate_basel %>%
#'   dplyr::filter(Year != 2017 & Month != "Feb")
#' uts_data_check_and_fill_w_na(data, key = "Measure")
#' @export
uts_data_check_and_fill_w_na <- function(data, key = NULL) {

  # assert if names_data is subset of names of input data columns
  # => all columns exist for pivot_wider() and for select()
  names_data <- c("Year", "Month")
  assert_names(names_data, subset.of = names(data))

  data <- data %>%
    unite("Year_Month", Year, Month, sep = "-", remove = FALSE) %>%
    mutate(Year_Month = tsibble::yearmonth(Year_Month))
  # yearmonth("1997-1") = yearmonth("1997-Jan") = "1997 Jan"
  # date(yearmonth("1997-Jan")) = "1997-01-01"

  # fill_gaps with NAs since (since ts objects don't allow missing years)
  data_tsbl <-
    tsibble::as_tsibble(data, index = Year_Month, key = key)
  (data_tsbl %>% has_gaps())   # Check for gaps
  (data_tsbl %>% scan_gaps())  # Reveal
  (data_tsbl %>% count_gaps()) # Summarise - number of gaps from - to
  data_tsbl <- data_tsbl %>% fill_gaps() # Fill in time gaps
  (data_tsbl %>% has_gaps())   # Check for gaps

  return(data_tsbl)
}


#' Converts stlplus() output object to a tibble object
#'
#' New tibble object has NA replacements (if any) by
#' interpolation of seasonal plus trend values.
#'
#' @param stlplus_data object of class "stlplus"
#'
#' @return
#' `# A tibble: 4,344 x 11` \cr
#' `Year_Month Year Month count Raw Interpolated Trend Seasonal Remainder Seasonal_Adjust` \cr
#' `<mth> <dbl> <fct> <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>` \cr
#' `  1   1659 Jan  1659 Jan   3     3   2.84  9.41 -6.57 0.159 9.57` \cr
#'
#' Easy plot with pkgTS::ggts_decomp()
#'
#' @seealso [stlplus::stlplus] and [ggts_decomp].
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
#' economics_ts <- stats::ts(economics_tsbl$uempmed,
#'   start = start_ts, frequency = 12)
#' # convert to tibble object
#' economics_stlplus <- stlplus::stlplus(economics_ts, s.window = 27, t.window = 1201)
#' #
#' uts_stlplus_as_tibble(economics_stlplus)
#' ggts_decomp(uts_stlplus_as_tibble(economics_stlplus))
#' @export
uts_stlplus_as_tibble <- function(stlplus_data) {

  # stlplus_data
  # $data # output of stlplus()
  #      raw   seasonal     trend   remainder weights   sub.labels
  # 1    3.0 -6.5721836  9.412794  0.15938952       1  subseries 1
  #
  # $time
  # [1] 1659.000 1659.083 1659.167 1659.250

  stl_year <- as.integer(floor(stlplus_data$time))
  stl_month <- as.integer(round((stlplus_data$time %% 1) * 12 + 1))

  stlplus_new <- as_tibble(
    tibble::rownames_to_column(as.data.frame(stlplus_data$data), var = "Index")) %>%
    rename(Raw = raw, Seasonal = seasonal, Trend = trend,
           Remainder = remainder) %>%
    mutate(Year = stl_year,
           Month = factor(stl_month))
  levels(stlplus_new$Month) <- month.abb

  stlplus_new <- stlplus_new %>%
    unite("Year_Month", Year, Month, sep = "-", remove = FALSE) %>%
    mutate(Year_Month = tsibble::yearmonth(Year_Month),
           Interpolated = Seasonal + Trend,
           Remainder = case_when(is.na(Remainder) ~ 0, TRUE ~ Remainder),
           # since count repl. by interpolated => new remainder for ex NA: = 0
           Seasonal_Adjust = Trend + Remainder, # = count - seasonal)
           NA_replace = case_when(is.na(Raw) ~ Interpolated, TRUE ~ NA_real_),
           # to allow plots line(Raw) + line/points(NA_replace) = count line
           count = case_when(is.na(Raw) ~ Interpolated, TRUE ~ Raw)) %>%
    dplyr::select(Year_Month, Year, Month, count, Raw, Interpolated,  Trend,
                  Seasonal, Remainder, Seasonal_Adjust, NA_replace)

  return(stlplus_new)
}


#' Replace NA by interpolation separate for each time series
#'
#' If there are there any NAs in monthly data set
#' => replace NA by interpolation separate for each time series
#'
#' @param data A data frame (column count)
#' @param freq season lenght, e.g. freq = 12 (1 year) for monthly temperature data
#' @return data frame
#'
#' if no NAs exist in count column => return unchanged data frame
#'
#' otherwise: \cr
#' `A tibble: 120 x 9`\cr
#' `City Measure Year_Month  Year Month count   Raw NA_replace`\cr
#' `<chr> <fct> <mth> <dbl> <fct> <dbl> <dbl> <dbl>`\cr
#'
#' data frame with columns
#'
#'  * count (updated) = orig and interplolated counts (replaced NAs, if any)
#'  * Raw (added) = orig counts (w/o replaced NAs)
#'  * NA_replace (added) = NAs (if count value exists) and
#'  interplolated counts (replaced NAs, if any)
#'  * for plots w/ & w/o replaced counts
#
#' @references
#' **Schlittgen, Rainer - De Gruyter**, *Angewandte Zeitreihenanalyse mit R*,
#' 2015 ISBN 978-3-11-041398-4
#' @examples
#' dplyr::slice(uts_interpolate_na(monthly_climate_giessen, freq =12), 75:90)
#' @export
uts_interpolate_na <- function(data, freq) {

  n_na <- tibble::as_tibble(data) %>%
    summarise(nr_na = sum(is.na(count))) %>% sum()

  if (n_na == 0)  return(data)  # nothing to be done
                               # check needed for uts_missls()

  # ensure that first and last yearl data are not NAs
  first_year_w_Jan_count <- data %>%
    dplyr::filter(Month == "Jan" & !is.na(count)) %$%
    min(Year)
  last_year_w_Dec_count <- data %>%
    dplyr::filter(Month == "Dec" & !is.na(count)) %$%
    max(Year)

  data <- data %>%
    dplyr::filter(Year >= first_year_w_Jan_count & Year <= last_year_w_Dec_count)
  n_na  <- tibble::as_tibble(data) %>% summarise(nr_na = sum(is.na(count)))
  if (n_na == 0)  return(data)  # nothing more to be done
                                # check needed for uts_missls()

  data_ts <- data %$%
    stats::ts(count, start=c(first_year_w_Jan_count, 1), frequency = freq)
  data_ts <- uts_missls(data_ts, p=3, 1)

  # to allow plots line(Raw) + line/points(NA_replace) = count line
  # and easy check between
  #    previous NAs in RAW (old count) and
  #    replaced NAs in updated count
  #    column Raw, NA_replace and Interpolated (return of uts_missls) added
  data <- data %>% mutate(Raw = count,
                   Interpolated = as.vector(data_ts),
                   NA_replace = case_when(is.na(Raw) ~ Interpolated,
              # to allow plots line(Raw) + line/points(NA_replace) = count line
                                          TRUE ~ NA_real_),
                   count = case_when(is.na(Raw) ~ Interpolated,
                                     TRUE ~ Raw)) %>%
    select(-Interpolated)
}


#'  uts_missls
#'
#'  missls         : filling in missing values in time series \cr
#'  ldrec          : Levinson-Durbin recursion \cr
#'  interpol       : auxiliary function for missls \cr
#'
#' @details
#'  Purpose : Minimum Mean Square Error Interpolator to fill missings
#'            using LS approach
#'
#'  Format  :  y = missls(x,p=0,tol=0.001,theo=0)
#'
#'  Input   :  x = (n,1)-vector, time series with missings \cr
#'             p = scalar, 0, or prespecified order of AR modell
#'
#'  Output  :  y = (n,1) vector, completed time series
#'
#'  Remarks :  first and last observation mut not be missing
#'             tolerance can be set through variable tol
#'             it enters via tol*sd(x,na.rm=TRUE)
#'             prespecified  iacf can be incorporated trough variable
#'             theo = (k,1)-vector, prespecified iacf (starting at lag 1)
#'
#' @param x value
#' @param p value
#' @param tol tolereance value
#' @param theo value
#' @references
#' Source: Rainer Schlittgen (01.10.2014) tsutil.r missls()
#'
#' Brubacker, S. and Wilson, G. (1976): Interpolating time series
#'            with applications to the estimation of holiday effects
#'            on electricity demand
#'            Journal of the Royal Statistical Society, C, 25, 107-116
uts_missls <- function(x, p=0, tol=0.001, theo=0){

  n <- length(x)
  if(length(x[!is.na(x)]) == n){ stop("no missing observation")  }
  if(is.na(x[1])|is.na(x[n])){ stop("First and last observation must not be missing")  }

  mu <- mean(x, na.rm = TRUE)
  xcent <- x-mu
  tol <- tol*sd(x, na.rm = TRUE)

  if (theo == 0){                    # fitting of an AR[p] model
    if (p == 0) { p <- trunc(n/10) }
    # estimation of ACF
    indexf <- c(1:n)
    indexf <- indexf[is.na(x)]
    y <- xcent
    y[indexf] <- 0
    g <- 1*(!is.na(xcent))
    ycov <- stats::acf(y,p,type="covariance",demean=FALSE,plot=FALSE)
    ycov <- ycov$acf
    gcov <- stats::acf(g,p,type="covariance",demean=FALSE,plot=FALSE)
    gcov <- gcov$acf
    xcov <- ycov/gcov
    xcor <- xcov/xcov[1]
    mat <- uts_ldrec(xcor)               # Compute Levinson-Durbin recursion
    a <- mat[p,1:p]                      # select AR coefficients
    rho <- as.vector(stats::ARMAacf(ma = -a, lag.max = p))     #  iacf
    rho <- rho[-1]
  }else{
    rho <- theo
    p <- length(rho)
  }
  wieder <- 0
  abbruch <- 0
  while(abbruch == 0){
    z <- interpol(rho,xcent)
    if(theo == 0){
      aneu <- stats::ar(z, aic = FALSE, order.max = p, method= "yule-walker")
      aneu <- aneu$ar
      if (max(abs(a-aneu)) < tol) { abbruch <- 1  }
      else{
        a <- aneu
        rho <- as.vector(stats::ARMAacf(ma = -a, lag.max = p)) # new iacf
        rho <- rho[-1]
      }
    }else{ abbruch <- 1 }
    wieder <- wieder+1
    if(wieder > 20)  {abbruch <- 1}
  }
  out <- z + mu
  return(out)
}


#' Levinson-Durbin recursion
#'
#' Levinson-Durbin recursion for determing all coefficients a(i,j),
#'   mat <- ldrec(a)\cr
#'   1<=i<=j<=maxlag (=p)\cr
#'   input  : a is (p+1,1)-vector of acf of a time series: acov(0),...,acov(p)
#'                              or  1,acor(1),..,acor(p) \cr
#'   output : (p,p+2)-matrix with coefficients in lower triangular,
#'            pacf in colum p+2 and Q(p) in colum p+1\cr
#' @param a value
#' @references
#'  Source: Rainer Schlittgen (01.10.2014) tsutil.r ldrec()
uts_ldrec <- function(a){
  p <- length(a)-1
  mat <- matrix(0,p,p+2)
  acor <- a/a[1]
  acor <- acor[-1]
  mat[1,1] <- acor[1];  mat[1,p+1] <- 1-acor[1]^2;   mat[1,p+2] <- acor[1]
  i <- 1
  while(i < p){
    mat[i+1,i+1] <- (acor[i+1] - sum(mat[i,1:i]*acor[i:1]))/mat[i,p+1]
    mat[i+1,1:i] <- mat[i,1:i]-mat[i+1,i+1]*mat[i,i:1]
    mat[i+1,p+2] <- mat[i+1,i+1]
    mat[i+1,p+1] <- mat[i,p+1]*(1-mat[i+1,p+2]^2)
    i <- i+1;
  }
  mat
}

#' interpol function
#'
#' auxiliary function for missls
#'
#' @param rho value
#' @param xcent value
interpol <- function(rho,xcent){

  n <- length(xcent)
  p <- length(rho)
  fehl <- c(1:n)
  fehl <- fehl[is.na(xcent)]
  m <- length(fehl)
  z <- xcent
  z[fehl] <- 0
  zt <- rep(0,m)                     # \tilde{z}
  s <- fehl[1]
  k <- 1
  while( k <= m ){
    i <- fehl[k]-s
    bis1 <- min(c(n-i-s,p))
    bis2 <- min(c(s+i-1,p))
    zt[k] <- -( sum(rho[1:bis1]*z[(s+i+1):(bis1+s+i)])
                + sum(rho[1:bis2]*z[(s+i-1):(s+i-bis2)]) )
    k <- k+1
  }
  mat <- diag(rep(1,m))
  k <- 1
  while( k < m ){
    dist <- fehl[(k+1):m]-fehl[k]
    if( min(dist) <= p ){
      lp <- c(1:length(dist))
      lp <- lp[(dist > 0)&(dist <= p)]
      mat[k,k+lp] <- t(rho[dist[lp]])
      mat[k+lp,k] <- t(mat[k,k+lp])
    }
    k <- k+1
  }
  neu.lm <- stats::lm.fit(mat,zt)
  z[fehl] <- neu.lm$coefficients
  return(z)
}
