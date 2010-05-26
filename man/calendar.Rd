\name{calendar}
\alias{is.leap}
\alias{ndays.year}
\alias{ndays.month}
\alias{date2dow}
\alias{date2doy}
\alias{doy2date}
\title{calendar functions}
\description{These should be mostly self-explanatory. Note that all these functions are vectorised and that all enumerations start at 1 (not 0).}
\usage{
is.leap (year)
ndays.year (year)
ndays.month (year, month)
date2dow (year, month, day)
date2doy (year, month, day)
doy2date (year, doy)
}
\arguments{
  \item{year}{The (full) year, e.g. 2000, 2010, 1924.}
  \item{month}{The month, in 1 to 12.}
  \item{day}{Day of the month, in 1 to (28 to 31, depending on the month).}
  \item{doy}{Day of the year, in 1 to (365 or 366, depending on the year).}
}



