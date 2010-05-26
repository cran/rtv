\name{timeplot}
\alias{plot.rtv}
\alias{timeaxis}
\title{time plots}
\description{Functions for creating time plots. For plot.rtv: If only x is provided, then an ecdfplot is produced. If both x and y are provided, then a lineplot is produced. If cycle is missing (the default), then a plot is produced with a formatted time axis. If cycle is specified, then the x values are replaced by their cycle positions. Further arguments can be included for plot.default, such as main. For timeaxis: Refer to the standard axis function and drtvf. Further arguments can be included for drtvf.}
\usage{
\method{plot}{rtv}(x, y, \dots, at, cycle)
timeaxis (side, x, \dots, at, n=7)
}
\arguments{
\item{x}{For plot.rtv, an rtv object (noting that drtv objects are converted to crtv objects using the default origin and unit). For timeaxis, a crtv object.}
\item{y}{Optionally, a numeric vector.}
\item{cycle}{Optionally, a crtv unit.}
\item{side}{The side to place the axis.}
\item{at}{Positions of the tick marks.}
\item{n}{The number of points on the axis, ignored if at provided.}
\item{\dots}{.}
}




