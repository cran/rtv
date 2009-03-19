\name{timestring}
\alias{timestring}
\alias{timestring.format}
\title{Formatted Time Strings}
\description{The function timestring is for creating human-readable formatted time strings. The function timestring.format is a convenience function for returning either a date or date-tod format.}
\usage{
timestring (x, date=getOption ("rtv.print.date"),
    outformat=timestring.format (date), \dots)
timestring.format (date=TRUE)
}
\arguments{
\item{x}{An rtv object.}
\item{date}{If true, date format, else date-tod format. Ignored if outformat provided.}
\item{outformat}{A format string.}
\item{\dots}{Ignored.}
}

