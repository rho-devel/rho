\name{D_draw.key}
\alias{draw.key}
\title{Produce a Legend or Key}
\description{
  Produces (and possibly draws) a Grid frame grob which is a legend (aka 
  key) that can be placed in other Grid plots.
}
\usage{
draw.key(key, draw=FALSE, vp=NULL, \dots)
}
\arguments{
  \item{key}{ A list determining the key. See documentation for
    \code{xyplot}, in particular the section describing the \code{key}
    argument, for details.
  }
  \item{draw}{ logical, whether the grob is to be drawn. }
  \item{vp}{ viewport }
  \item{\dots}{ignored}
}
\value{
  A Grid frame object (that inherits from \sQuote{grob}).
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{xyplot}}}
\keyword{dplot}

