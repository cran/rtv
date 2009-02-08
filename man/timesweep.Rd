\name{timesweep}
\alias{length.rtv}
\alias{sort.rtv}
\alias{subset.rtv}
\alias{[.drtv}
\alias{[.crtv}
\alias{mean.rtv}
\alias{diff.rtv}
\alias{min.rtv}
\alias{max.rtv}
\alias{range.rtv}
\alias{+.rtv}
\alias{-.rtv}
\alias{print.rtv}
\alias{sample.rtv}
\alias{rtv.incr}
\alias{timesweep}
\alias{c.rtv}
\title{Functions of rtv Objects}
\description{Most of the functions here are methods that implement a standard R generic (or primitive), often by calling the function timesweep which applies a standard function f to an rtv object x. The functions +.rtv and -.rtv call the function rtv.incr which adds a numeric to an rtv object, but not an rtv object to an rtv object. The function c.rtv is only designed to take objects that can be coerced to numeric and bases the return type on the first object. The function sample.rtv is not currently an R method as such, and must be called as sample.rtv (in order for an rtv object to be returned). Unlike the other functions diff.rtv returns a numeric, and so does range.rtv if diff is true.}
\usage{
\method{length}{rtv}(x, \dots)
\method{sort}{rtv}(x, \dots)
\method{subset}{rtv}(x, v, \dots)
\method{[}{drtv}(x, i)
\method{[}{crtv}(x, i)
\method{mean}{rtv}(x, \dots)
\method{diff}{rtv}(x, \dots)
\method{min}{rtv}(x, \dots)
\method{max}{rtv}(x, \dots)
\method{range}{rtv}(x, diff = FALSE, \dots)
\method{+}{rtv}(x1, x2 = NULL)
\method{-}{rtv}(x1, x2 = NULL)
\method{print}{rtv}(x, date = getOption ("rtv.print.date"), \dots)
sample.rtv(x, \dots)
rtv.incr(x1, x2, unit = attr(x1, "unit"))
timesweep(f, x, \dots)
c.rtv(\dots)
}
\arguments{
\item{f}{A function to apply.}
\item{x}{An rtv object.}
\item{i}{Indices.}
\item{v}{Logical vector.}
\item{x1}{An rtv object or a numeric vector.}
\item{x2}{An rtv object or a numeric vector.}
\item{unit}{A "day" if x is drtv object, the crtv unit otherwise.}
\item{diff}{If true, return the difference.}
\item{date}{If true (and formatted printing enabled), print using date format.}
\item{\dots}{For c.rtv, a list of rtv objects, otherwise further arguments for the child call, e.g. na.rm=TRUE.}
}

