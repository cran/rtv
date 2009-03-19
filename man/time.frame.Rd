\name{time.frame}
\alias{time.frame}
\alias{as.data.frame.rtv}
\title{time.frame Objects}
\description{time.frame is called in exactly the same way as data.frame, except the the return value is a time.frame object (which is a subclass of data.frame). crtv objects can be included in either time.frame objects or regular data.frame objects. Methods are likely to be implemented for time.frame objects in future releases. At present only aggregate is implemented. In general, the user shouldn't need to call as.data.frame.rtv directly.}
\usage{
time.frame (\dots)
as.data.frame.rtv  (x, \dots)
}
\arguments{
\item{x}{An rtv object.}
\item{\dots}{For time.frame, same as data.frame. For as.data.frame.rtv, ignored.}
}

