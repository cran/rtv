\name{as.rtv}
\alias{as.rtv}
\alias{as.drtv}
\alias{as.crtv}
\alias{as.double.rtv}
\alias{as.Date.rtv}
\alias{as.POSIXlt.rtv}
\alias{as.POSIXct.rtv}
\title{Coercing rtv Objects}
\description{Functions to coerce different objects to and from rtv objects. Note that at present as.rtv is the same as as.drtv. Also note that crtv.crtv and as.crtv on a crtv object, do not necessarily return the same object. By default crtv.crtv uses default values for the origin and unit, where as if as.crtv is invoked on a crtv object, it will always return the same crtv object.}
\usage{
as.rtv(x)
as.drtv(x)
as.crtv(x)
as.double.rtv(x, std = FALSE, ...)
as.Date.rtv(x, \dots)
as.POSIXlt.rtv(x, \dots)
as.POSIXct.rtv(x, \dots)
}
\arguments{
\item{x}{Object to be coerced.}
\item{std}{Standardize a crtv object before stripping attributes. Only relevant if x is a crtv object.}
\item{\dots}{Ignored.}
}

