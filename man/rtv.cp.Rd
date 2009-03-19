\name{rtv.cp}
\alias{rtv.cp}
\alias{rtv.tod}
\title{Cycle Positions over Time}
\description{The function rtv.cp returns the fractional portion of a crtv object as a numeric. This represents the position in some (time unit based) cycle. e.g. The position in a daily cycle. The function rtv.tod is a wrapper function for the time of the day. i.e. The unit is day.}
\usage{
rtv.cp (x, unit)
rtv.tod (x)
}
\arguments{
\item{x}{A crtv object.}
\item{unit}{The cycle unit. If missing, the unit of x is used.}
}

