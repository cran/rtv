\name{calendar}
\alias{is.leap}
\alias{ndays.year}
\alias{ndays.month}
\alias{cumdays.month}
\alias{date.to.dow}
\alias{date.to.doy}
\alias{doy.to.date}
\alias{dow.correction}
\title{Convenience Functions for Dates}
\description{Most of these are self-explanatory. Days of week and days of year start from 1, and dow.correction replaces 0 with 7. Arguments need not be of the same length.}
\usage{
is.leap(year)
ndays.year(year)
ndays.month(year, month)
cumdays.month(year, month, inclusive = TRUE)
date.to.dow(year, month, day)
date.to.doy(year, month, day)
doy.to.date(year, doy)
dow.correction(dow)
}
\arguments{
  \item{year}{}
  \item{month}{}
  \item{day}{Day of the month.}
  \item{dow}{Day of the week. Mon=1, Tue=2, \dots, Sun=7.}
  \item{doy}{Day of the year.}
  \item{inclusive}{Include or exclude the given month.}
}

