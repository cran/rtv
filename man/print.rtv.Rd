\name{print.rtv}
\alias{print.rtv}
\alias{format.drtv}
\alias{as.character.drtv}
\alias{drtvf}
\title{print and format rtv objects}
\description{Functions for printing and formatting rtv objects. Note that the behaviour of both print.rtv and format.rtv depends on rtv options. The function drtvf (x), is a shortcut for format (drtv (x) ), and format.drtv and as.character.drtv do the same thing.}
\usage{
\method{print}{rtv}(x, \dots)
\method{format}{drtv}(x, date=getOption ("rtv.date"), style, \dots)
\method{as.character}{drtv}(x, \dots)
drtvf (x, ...)
}
\arguments{
\item{x}{An rtv object.}
\item{date}{Whether to include the date in formatting, ignored if style provided.}
\item{style}{The format style, as a single character.}
\item{\dots}{.}
}




