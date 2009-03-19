\name{unitstring}
\alias{monthstring}
\alias{dowstring}
\alias{unitstring}
\title{String Representations of Months and Weekdays}
\description{The functions monthstring and dowstring, replace an integer representation of months and weekdays respectively with a string representation. The both call unitstring.}
\usage{
monthstring (x, \dots)
dowstring (x, \dots)
unitstring (x, levs, case="title", nletters=3)
}
\arguments{
\item{x}{A vector of integers.}
\item{levs}{Level names.}
\item{case}{Sets the cases, "upper", "lower" or anything else, for title case.}
\item{nletters}{Number of letters, defaults to 3, set to NA for all.}
\item{\dots}{Arguments for unitstring.}
}

