\name{rtv.incr}
\alias{rtv.incr}
\title{Incrementing rtv Objects}
\description{This function increments an rtv object by a numeric vector defining a certain number of time unit events.}
\usage{rtv.incr (x1, x2, unit=attr (x1, "unit") )}
\arguments{
\item{x1}{An rtv object.}
\item{x2}{A numeric vector.}
\item{unit}{The type of time unit event. If a drtv object, and missing, uses the rtv default, if crtv and missing, uses the same unit as the crtv object.}
}

