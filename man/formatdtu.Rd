\name{formatdtu}
\alias{formatmonth}
\alias{formatdow}
\alias{formatdtu}
\title{format discrete time units}
\description{The functions formatmonth and formatdow take either an rtv object or an integer vector, and produce a character vector. The function formatdtu, is designed to support these two functions, however can be used for other units.}
\usage{
formatmonth (x, \dots)
formatdow (x, \dots)
formatdtu (x, levels, case="title", nchars=3)
}
\arguments{
\item{x}{For formatmonth and formatdow, either an rtv object or an integer vector. For formatdtu, an integer vector only.}
\item{levels}{Level names.}
\item{case}{The case, set to "upper" or "lower" for their respective cases.}
\item{nchars}{Number of characters, defaults to 3, set to NA for all.}
\item{\dots}{.}
}




