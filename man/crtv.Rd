\name{crtv}
\alias{crtv}
\alias{crtv.drtv}
\alias{crtv.crtv}
\alias{crtv.character}
\alias{crtv.Date}
\alias{crtv.POSIXlt}
\alias{crtv.POSIXct}
\alias{crtv.default}
\title{Continuous Random Time Variable Objects}
\description{Functions for creating continuous random time variable, crtv, objects.}
\usage{
crtv(\dots)
\method{crtv}{drtv}(x, \dots)
\method{crtv}{crtv}(x, clone=FALSE, relative=FALSE,
    origin=NULL, unit=NULL, \dots)
\method{crtv}{character}(x, date=getOption ("rtv.read.date"),
    informat=timestring.format (date), ...)
\method{crtv}{Date}(x, \dots)
\method{crtv}{POSIXlt}(x, \dots)
\method{crtv}{POSIXct}(x, relative=FALSE,
    origin=getOption ("rtv.default.origin"),
    unit=getOption ("rtv.default.unit"), \dots)
\method{crtv}{default}(x,
    origin=getOption ("rtv.default.origin"),
    unit=getOption ("rtv.default.unit"), \dots)
}
\arguments{
\item{x}{A seed object.}
\item{clone}{If origin and unit unspecified, use those from seed crtv object.}
\item{relative}{If true, the origin will be min (x, na.rm=TRUE).}
\item{origin}{The origin, ignored if relative is true.}
\item{unit}{The unit.}
\item{date}{Date or date-tod format. Ignored if informat provided.}
\item{informat}{The string format.}
\item{\dots}{Arguments for other crtv constructors.}
}

