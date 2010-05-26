\name{drtv}
\alias{drtv}
\alias{drtv.drtv}
\alias{drtv.crtv}
\alias{drtv.character}
\alias{drtv.Date}
\alias{drtv.POSIXlt}
\alias{drtv.POSIXct}
\alias{drtv.default}
\alias{drtvs}
\alias{drtvx}
\title{drtv objects}
\description{Functions for creating discrete (calendar-based) random time variable objects.}
\usage{
drtv(\dots)
\method{drtv}{drtv}(x, \dots)
\method{drtv}{crtv}(x, \dots)
\method{drtv}{character}(x, \dots, date=TRUE, hour=6, style)
\method{drtv}{Date}(x, \dots, hour=6)
\method{drtv}{POSIXlt}(x, \dots)
\method{drtv}{POSIXct}(x, \dots)
\method{drtv}{default}(year=2000, month=1, day=1,
    hour=6, minute=0, second=0,
    dow=1, doy=1, \dots, validate=TRUE, round=FALSE)
drtvs (x, \dots)
drtvx (x, \dots)
}
\arguments{
\item{x}{.}
\item{year}{.}
\item{month}{.}
\item{day}{.}
\item{hour}{.}
\item{minute}{.}
\item{second}{.}
\item{dow}{.}
\item{doy}{.}
\item{validate}{.}
\item{round}{.}
\item{date}{.}
\item{style}{.}
\item{\dots}{.}
}

