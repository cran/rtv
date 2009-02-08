\name{unit.string}
\alias{month.string}
\alias{dow.string}
\alias{unit.string}
\title{String Representations of Months and Weekdays}
\description{Replaces an integer representation of months and weekdays with a string representation, along with a number of formatting options.}
\usage{
month.string(x, \dots)
dow.string(x, \dots)
unit.string(x, levs, case = "title", nletters = 3)
}
\arguments{
\item{x}{A vector of integers.}
\item{levs}{Level names.}
\item{case}{Sets the cases, "upper", "lower" or anything else, for title case.}
\item{nletters}{Number of letters, defaults to 3, set to NA for all.}
\item{\dots}{Arguments for unit.string.}
}

