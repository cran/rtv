\name{format.rtv}
\alias{format.rtv}
\alias{explicit.format}
\alias{default.format}
\title{Formatting rtv Objects}
\description{Functions for formatting rtv objects. Note that calling format.rtv (my.rtv.object) does not format rtv objects by default. Either set the option rtv.explicit.format = TRUE, or call explicit.format instead. Note there are further functions for formatting, see unit.string.}
\usage{
\method{format}{rtv}(x, \dots)
explicit.format(x,
    date = TRUE, outformat = default.format(date), \dots)
default.format(date = TRUE)
}
\arguments{
\item{x}{An rtv object.}
\item{date}{Date format if true, date-time format otherwise, ignored if outformat provided.}
\item{outformat}{Format string.}
\item{\dots}{For format.rtv, used calling explicit.format. For explicit.format, ignored.}
}

