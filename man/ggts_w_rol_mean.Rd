% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggts_climate.R
\name{ggts_w_rol_mean}
\alias{ggts_w_rol_mean}
\title{Generate ggplot (Time-)Series Line Object with Rolling mean and Smooth line}
\usage{
ggts_w_rol_mean(data, time = Year_Month, y = count, span)
}
\arguments{
\item{data}{data.frame with x (e.g. time line) and y data to be displayed,
e.g. ggplot_w_rol_mean(weather_long, Year_Month, Temperature, span = span).
time : numeric string (also of data and Time classes) as index for
column name.
y : numeric string as index for y column name.}

\item{time}{data frame column for values of x axes.}

\item{y}{data frame column for values of y axes.}

\item{span}{integer width of the rolling window.}
}
\value{
plot object of mode `plot
}
\description{
Calculates rolling mean with stats::filter(y, filter = rep(1/span, span)
and adding smooth line with geom_smooth(method = "loess").
}
\details{
The data input must be a valid data.frame.

data input format (e.g. Month in long format) \cr
\verb{   Year_Month ...   count} \cr
\verb{   <mth> ...   <dbl>} \cr
\verb{1   1890 Jan ...     2.8} \cr
\verb{2   1890 Feb ...    -1.7} \cr
\code{:} \cr
\verb{n   2019 Dec ...     1.4} \cr
}
\examples{
ggts_w_rol_mean(ggplot2::economics, date, uempmed, span = 7)
}
\seealso{
\link[stats:filter]{stats::filter} and \link[ggplot2:ggplot]{ggplot2::ggplot}
}
\keyword{misc}
