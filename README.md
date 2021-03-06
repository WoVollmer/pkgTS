
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgTS

<!-- badges: start -->

<!-- badges: end -->

The goal of R package `pkgTS` is to provide functions for time series
analysis, e.g. usable for

  - climate temperature, precipitation or atmospheric CO\_2
    concentration analysis
  - corona virus spread analysis.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("WoVollmer/pkgTS")
```

## Example

This is a basic example which shows how to plot corona time series data:

``` r
library(pkgTS)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## basic example code
ggts_cum_daily(corona_data, country = "Germany", weeks = 6)
#> Scale for 'x' is already present. Adding another scale for 'x', which will
#> replace the existing scale.
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# note for display on GitHub:
#  resulted plot files in /man/figures to be committed and pushed to package
```

## Corona Virus Dashboard

The **`Corona Virus Dashboard`** provides analysis of the time series
data provided by the **Johns Hopkins University** on GitHub. For links
and references see the Dashboard-file *Corona Virus Dashboard Web Site*
rsp. RMD-file *Corona\_Virus\_TS\_Dashboard.Rmd*.

The
[Corona\_Virus\_TS\_Dashboard.Rmd](https://github.com/WoVollmer/R-TimesSeriesAnalysis/tree/master/Corona-Virus)
file is stored on GitHub repository.

The [Corona Virus Dashboard Web
Site](https://wovollmer.github.io/github.io/) is published as **GitHub
page**
