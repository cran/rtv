\name{round.rtv}
\alias{round.drtv}
\alias{round.crtv}
\title{Rounding rtv Objects}
\description{For drtv objects, rounds to the nearest 1000th of a second. For crtv objects, wraps the regular round function. These may be changed in the near future.}
\usage{
\method{round}{drtv}(x, \dots)
\method{round}{crtv}(x, \dots)
}
\arguments{
\item{x}{An rtv object.}
\item{\dots}{For drtv, ignored. For crtv, further rounding arguments.}
}

