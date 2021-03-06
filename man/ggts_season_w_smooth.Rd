% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggts_climate.R
\name{ggts_season_w_smooth}
\alias{ggts_season_w_smooth}
\title{Provides Monthly data plot (e.g. Temperature, Precipitation, ...)}
\usage{
ggts_season_w_smooth(data, time = Year_Month, y = count, season = Month)
}
\arguments{
\item{data}{data frame with monthly data (long format)}

\item{time}{data frame column for values of x axes.}

\item{y}{data frame column for values of y axes.}

\item{season}{col aestetic for points and loess smooth}
}
\value{
plot object of mode `plot
}
\description{
Monthly plot will be provided wit loess smooth line (no facetting)
}
\examples{
ggts_season_w_smooth(monthly_climate_basel)  +
  ggplot2::facet_wrap(~Measure, ncol = 1, scales = "free")
}
