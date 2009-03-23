\name{as.rtv}
\alias{as.drtv}
\alias{as.crtv}
\alias{as.double.rtv}
\alias{as.Date.rtv}
\alias{as.POSIXlt.rtv}
\alias{as.POSIXct.rtv}
\alias{as.character.rtv}
\title{Coercing rtv Objects}
\description{Functions to coerce different objects to and from rtv objects. The function as.character.rtv is the same as timestring.}
\usage{
as.drtv (x)
as.crtv (x)
as.double.rtv (x, std=FALSE, \dots)
as.Date.rtv (x, \dots)
as.POSIXlt.rtv (x, \dots)
as.POSIXct.rtv (x, \dots)
as.character.rtv (x, \dots)
}
\arguments{
\item{x}{Object to be coerced.}
\item{std}{Standardize a crtv object to the default origin and unit, before stripping attributes. Only relevant if x is a crtv object.}
\item{\dots}{Ignored, except for as.character.rtv.}
}

