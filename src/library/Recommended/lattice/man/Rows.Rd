\name{G_Rows}
\alias{Rows}
\title{Extract rows from a list}
\description{
  Convenience function to extract subset of a list. Usually used in
  creating keys.
}
\usage{
Rows(x, which)
}
\arguments{
  \item{x}{ list with each member a vector of the same length }
  \item{which}{ index for members of \code{x}}
}
\value{
  A list similar to \code{x}, with each \code{x[[i]]} replaced by
  \code{x[[i]][which]}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{xyplot}}, \code{\link{Lattice}}}
\keyword{utilities}
















