\name{crtv}
\alias{crtv}
\alias{crtv.drtv}
\alias{crtv.crtv}
\alias{crtv.character}
\alias{crtv.Date}
\alias{crtv.POSIXlt}
\alias{crtv.POSIXct}
\alias{crtv.default}
\alias{crtvs}
\alias{crtvx}
\alias{crtvc}
\alias{seq.crtv}
\alias{crtvcp}
\title{crtv objects}
\description{Functions for creating continuous (real-based) random time variable objects.}
\usage{
crtv(\dots)
\method{crtv}{drtv}(x, origin, unit, \dots)
\method{crtv}{crtv}(x, origin, unit, \dots)
\method{crtv}{character}(x, origin, unit, \dots, date=TRUE, hour=6, style)
\method{crtv}{Date}(x, origin, unit, \dots, hour=6)
\method{crtv}{POSIXlt}(x, origin, unit, \dots)
\method{crtv}{POSIXct}(x, origin, unit, \dots)
\method{crtv}{default}(x, origin, unit, \dots)
crtvs (x, \dots)
crtvx (x, \dots)
crtvc (x, origin = x$origin, unit = x$unit)
\method{seq}{crtv}(a, b, n, ...)
crtvcp (x)
}
\arguments{
\item{x}{.}
\item{a}{.}
\item{b}{.}
\item{hour}{.}
\item{origin}{.}
\item{unit}{.}
\item{date}{.}
\item{style}{.}
\item{n}{.}
\item{\dots}{.}
}

