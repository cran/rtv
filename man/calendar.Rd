\name{calendar}
\alias{is.leap}
\alias{ndays.year}
\alias{ndays.month}
\alias{cumdays.month}
\alias{date.to.dow}
\alias{date.to.doy}
\alias{doy.to.date}
\alias{dow.correction}
\title{Calendar Operations}
\description{Most of these are self-explanatory. Arguments need not be the same length.}
\usage{
is.leap (year)
ndays.year (year)
ndays.month (year, month)
cumdays.month (year, month, inclusive=TRUE)
date.to.dow (year, month, day)
date.to.doy (year, month, day)
doy.to.date (year, doy)
dow.correction (dow)
}
\arguments{
  \item{year}{}
  \item{month}{}
  \item{day}{Day of the month.}
  \item{dow}{Day of the week. Mon->1, Tue->2, \dots, Sun->7.}
  \item{doy}{Day of the year. Jan01->1.}
  \item{inclusive}{Include or exclude the given month.}
}

