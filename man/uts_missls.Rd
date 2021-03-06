% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uts_climate.R
\name{uts_missls}
\alias{uts_missls}
\title{uts_missls}
\usage{
uts_missls(x, p = 0, tol = 0.001, theo = 0)
}
\arguments{
\item{x}{value}

\item{p}{value}

\item{tol}{tolereance value}

\item{theo}{value}
}
\description{
missls         : filling in missing values in time series \cr
ldrec          : Levinson-Durbin recursion \cr
interpol       : auxiliary function for missls \cr
}
\details{
Purpose : Minimum Mean Square Error Interpolator to fill missings
using LS approach

Format  :  y = missls(x,p=0,tol=0.001,theo=0)

Input   :  x = (n,1)-vector, time series with missings \cr
p = scalar, 0, or prespecified order of AR modell

Output  :  y = (n,1) vector, completed time series

Remarks :  first and last observation mut not be missing
tolerance can be set through variable tol
it enters via tol*sd(x,na.rm=TRUE)
prespecified  iacf can be incorporated trough variable
theo = (k,1)-vector, prespecified iacf (starting at lag 1)
}
\references{
Source: Rainer Schlittgen (01.10.2014) tsutil.r missls()

Brubacker, S. and Wilson, G. (1976): Interpolating time series
with applications to the estimation of holiday effects
on electricity demand
Journal of the Royal Statistical Society, C, 25, 107-116
}
