\name{time.aggregate}
\alias{aggregate.rtv}
\alias{aggregate.time.frame}
\alias{time.aggregate}
\title{Aggregation over Time}
\description{The function time.aggregate is a prototypical function and may be changed substantially in future releases, hence this documentation is being kept minimal.}
\usage{
\method{aggregate}{rtv} (x, \dots)
\method{aggregate}{time.frame} (x, \dots)
time.aggregate (x, unit, fs=NULL, labs=fs,
    period=TRUE, exhaustive=FALSE, \dots)
}
\arguments{
\item{x}{}
\item{unit}{The unit used for aggregation.}
\item{fs}{Character vector of functions, either form f, or f(varname), depending on object.}
\item{labs}{Labels for output, ignored unless fs provided.}
\item{period}{Output the period, ignored unless fs provided.}
\item{exhaustive}{Ignored.}
\item{\dots}{Arguments for timestring.}
}

