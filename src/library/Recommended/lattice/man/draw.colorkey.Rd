\name{D_draw.colorkey}
\alias{draw.colorkey}
\title{Produce a Colorkey for levelplot}
\description{
  Produces (and possibly draws) a Grid frame grob which is a colorkey
  that can be placed in other Grid plots. Used in levelplot
}
\usage{
draw.colorkey(key, draw=FALSE, vp=NULL)
}
\arguments{
  \item{key}{ A list determining the key. See documentation for
    \code{levelplot}, in particular the section describing the
    \code{colorkey} argument, for details.
  }
  \item{draw}{ logical, whether the grob is to be drawn. }
  \item{vp}{ viewport }
}
\value{
  A Grid frame object (that inherits from \code{"grob"})
}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{xyplot}}}
\keyword{dplot}

