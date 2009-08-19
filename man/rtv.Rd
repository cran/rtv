\name{rtv}
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
\alias{c.rtv}
\alias{floor.rtv}
\alias{ceiling.rtv}
\title{Methods for rtv Objects}
\description{Methods implemented for rtv objects. Most mathematical methods, will return an rtv object of the same class as the argument. The return type of c.rtv matches the first argument. The function diff, returns a numeric. The function sample.rtv is not an R method and may be removed or renamed in a future release.}
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
\method{floor}{rtv}(x, \dots)
\method{ceiling}{rtv}(x, \dots)
\method{range}{rtv}(x, diff=FALSE, \dots)
\method{+}{rtv}(x1, x2=NULL)
\method{-}{rtv}(x1, x2=NULL)
\method{print}{rtv}(x, \dots)
sample.rtv (x, \dots)
c.rtv (\dots)
}
\arguments{
\item{x}{An rtv object.}
\item{x1}{}
\item{x2}{}
\item{i}{Indices.}
\item{v}{Logical vector.}
\item{diff}{If true, return the difference.}
\item{\dots}{For c.rtv, a list of rtv objects, otherwise further arguments for the child call, e.g. na.rm=TRUE.}
}

