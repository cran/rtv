\name{whichperiod}
\alias{whichperiod}
\title{Which Time Period}
\description{Time periods (in this context) are taken to be equal length successive time intervals, represented by their starting values. This function takes a crtv object and returns the period that each realisation is in. The periods can be numbered or represented by timestrings. By default periods are represented as the number of periods since the origin.}
\usage{whichperiod (x, unit, format=FALSE, natural=FALSE, \dots)}
\arguments{
\item{x}{A crtv object.}
\item{unit}{The unit used for aggregation.}
\item{format}{Format the starting point of the period as a timestring.}
\item{natural}{If format is false and natural is true, use natural numbering 1, 2, \dots, n.}
\item{\dots}{Arguments for timestring.}
}

