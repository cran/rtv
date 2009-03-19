\name{drtv}
\alias{drtv}
\alias{drtv.drtv}
\alias{drtv.crtv}
\alias{drtv.character}
\alias{drtv.Date}
\alias{drtv.POSIXlt}
\alias{drtv.POSIXct}
\alias{drtv.default}
\title{Discrete Random Time Variable Objects}
\description{Functions for creating discrete random time variable, drtv, objects.}
\usage{
drtv(\dots)
\method{drtv}{drtv}(x, \dots)
\method{drtv}{crtv}(x, \dots)
\method{drtv}{character}(x, date=getOption ("rtv.read.date"),
    informat=timestring.format (date), \dots)
\method{drtv}{Date}(x, \dots)
\method{drtv}{POSIXlt}(x, \dots)
\method{drtv}{POSIXct}(x, \dots)
\method{drtv}{default}(year=2000, month=1, day=1,
    hour=0, minute=0, second=0,
    dow=1, doy=1, validate=TRUE, round=TRUE, \dots)
}
\arguments{
\item{x}{A seed object.}
\item{year}{}
\item{month}{}
\item{day}{}
\item{hour}{}
\item{minute}{}
\item{second}{}
\item{dow}{Day of week, ignored if validate is true.}
\item{doy}{Day of year, ignored if validate is true.}
\item{validate}{Process NAs, set dow and doy.}
\item{round}{Round the seconds, and if necessary other units.}
\item{date}{Date or date-tod format. Ignored if informat provided.}
\item{informat}{The string format.}
\item{\dots}{Arguments for other drtv constructors.}
}

